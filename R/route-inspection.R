# Title     : Route inspection functions
# Objective : Acquire more in-depth information about single routes
# Created by: Jonas Lieth
# Created on: 22.10.2021


#' Route inspection
#' @description \code{summarize_route} generates a range of summary tables and
#' values that provide an overview of a route.
#' @inheritParams inspect_route
#' @returns Object of type \code{route_summary} that contains information on
#' distances, durations, speed, elevation, detour factors as well as all
#' available extra information for the requested route.
#' @describeIn inspect_route
#'
#' @export

summarize_route <- function(source,
                            destination,
                            profile = get_profiles(),
                            ...) {
  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE)

  # Bring input data into shape
  source <- format_input_data(source)
  destination <- format_input_data(destination)

  verify_crs(source, crs = 4326)
  verify_crs(destination, crs = 4326)

  profile <- match.arg(profile)

  url <- get_ors_url()

  features <- list(attributes = TRUE, elevation = TRUE, extra_info = TRUE)
  options <- format_ors_options(append(features, list(...)), profile)

  res <- query_ors_directions(source = source,
                              destination = destination,
                              profile = profile,
                              units = "m",
                              geometry = TRUE,
                              options = options,
                              url = url)

  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = FALSE)

  # Custom summary tables
  geometry <- ors_multiple_linestrings(res, elev_as_z = FALSE)
  elevation <- attr(geometry, "elevation")

  distances <- calculate_distances(geometry)
  durations <- calculate_durations(res, distances$distance)
  speeds <- calculate_avgspeed(distances$distance, durations$duration)$avgspeed

  speeds_summary <- make_summary_table(unlist(speeds), distances)
  elevation_summary <- make_summary_table(elevation, distances)

  # ORS summary tables
  get_ors_summaries <- function(info_type) {
    summary <- extras[[info_type]]$summary[[1]]
    summary$value <- fill_extra_info(summary$value, info_type, profile)
    summary
  }

  extras <- res$features$properties$extras
  summaries <- sapply(names(extras), get_ors_summaries, simplify = FALSE)

  summaries <- sapply(summaries, function(summary) {
    summary <- aggregate(summary[, 2:3],
                         by = summary["value"],
                         sum)
    row.names(summary) <- summary$value
    summary$value <- NULL
    summary
  }, simplify = FALSE)

  # Append summary tables and set units
  elev_speeds <- list(elevation = elevation_summary, avgspeed = speeds_summary)
  summaries <- append(summaries,
                      elev_speeds,
                      after = 0)

  summaries <- lapply(summaries, function(s) {
    s["distance"] <- units::as_units(s$distance, "m")
    s
  })

  output <- list(
    distance = extract_ors_attribute(res, "distance"),
    duration = extract_ors_attribute(res, "duration"),
    detourfactor = extract_ors_attribute(res, "detourfactor"),
    ascent = extract_ors_attribute(res, "ascent"),
    descent = extract_ors_attribute(res, "descent"),
    speed = summary(speeds),
    elevation = summary(elevation),
    tables = summaries
  )

  units(output$distance) <- "m"
  units(output$duration) <- "s"

  class(output) <- "route_summary"
  output
}


#' @export

print.route_summary <- function(x, ncols = 3, ...) {
  cli::cli_text("{.strong Attributes}")
  for (n in names(x[1:5])) {
    name <- capitalizeChar(n)
    if (identical(n, "Detourfactor")) name <- "Detour factor"
    attribute_row <- paste(name, ": ", round(x[[n]], 2), "\n")
    cat(attribute_row)
  }

  cat("\n")

  for (n in names(x[6:7])) {
    name <- capitalizeChar(n)
    cli::cli_text("{.strong {name}}")
    print(x[[n]])
    cat("\n")
  }

  cli::cli_text("{.strong Additional infos}")

  tables <- x$tables

  # Get the maximum row length of the input tables
  row_lengths <- sapply(tables, function(tb) sapply(capture.output(tb), nchar))
  max_row_length <- max(unlist(row_lengths))

  # Get an iterator that splits up the tables at the indices defined by ncols
  table_iterator <- split(tables, ceiling(seq_along(tables) / ncols))

  # Iterate through each table row (as defined by ncols)
  for (t in table_iterator) {
    # Number of rows of each table
    rows <- sapply(t, nrow)

    # Cumulated number of rows + table name + column names + empty line
    border_indices <- cumsum(rows + 3)

    # Catch the output of print.data.frame and split it by table
    output <- capture.output(t)
    output_split <- split(output,
                          cumsum(is.element(seq_along(output),
                                            border_indices + 1)))

    # Get the maximum number of rows of one table row
    max_length <- max(sapply(output_split, length))

    # Iterate through each table of one table row
    op_vec <- lapply(output_split, function(op_vec) {
      # Remove empty character string
      op_vec <- op_vec[nchar(op_vec) != 0]

      # If the table name is shorter than the row length, fill with white space
      add_ws <- function(char, row_length, max_length) {
        if (row_length < max_length) {
          paste0(char, strrep(" ", max_length - row_length))
        } else char
      }

      name <- op_vec[1]
      name <- add_ws(name, nchar(name), max_row_length)

      columns <- op_vec[2]
      row_length <- nchar(columns)
      columns <- add_ws(columns, row_length, max_row_length)

      rows <- op_vec[seq(3, length(op_vec))]
      rows <- unlist(lapply(rows, add_ws, row_length, max_row_length))

      # Get the number of rows. If the table has less rows than the table
      # with the most rows, add rows and fill them with white space
      len <- length(rows)
      if (len < max_length) {
        rows[seq(len + 1, max_length - 2)] <- strrep(" ", max_row_length)
      }
      c(name, columns, rows)
    })

    # Transpose list to align corresponding rows from each table
    to_vector <- lapply(purrr::transpose(op_vec), unlist)
    # Piece together character strings
    to_string <- purrr::map(to_vector,
                            ~trimws(paste(., collapse = strrep(" ", 8)),
                                    which = "right"))
    to_string <- lapply(to_string, trimws, which = "right")
    row_string <- paste(to_string, collapse = "\n")
    cat(row_string)
    cat("\n")
  }
}
