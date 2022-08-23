#' Route inspection
#' @description Calls the directions service once to get a closer look at route
#' characteristics and attributes. \code{ors_summary} generates a range of
#' summary tables and values that provide an overview of a route.
#'
#' \code{ors_inspect} returns all line segments of a route along with a set
#' of additional attributes
#' @param source \code{[sf]}
#' 
#' Source dataset containing a single point geometry that shall be routed from.
#' @param destination \code{[sf]}
#' 
#' Destination dataset containing a single point geometry that shall be routed
#' to.
#' @param attributes \code{[list]}
#' 
#' List of attributes that summarize route characteristics.
#' This includes two values: \code{avgspeed} states the average vehicle speed
#' along the route, \code{detourfactor} indicates how much the route deviates
#' from a straight line. If \code{TRUE}, both values are included.
#' @param elevation \code{[logical]}
#' 
#' If \code{TRUE}, elevation data is included in the output.
#' @param extra_info List of keywords that add extra information regarding each
#' linestring segment of the output. Possible values include:
#' \itemize{
#'  \item steepness
#'  \item suitability
#'  \item surface
#'  \item waycategory
#'  \item waytype
#'  \item tollways (only for \code{driving-*})
#'  \item traildifficulty
#'  \item osmid (only for \code{wheelchair})
#'  \item roadaccessrestrictions (only for \code{driving-*})
#'  \item countryinfo (only for \code{driving-*})
#'  \item green (only for \code{walking}/\code{hiking})
#'  \item noise (only for \code{walking}/\code{hiking})
#' }
#' If \code{TRUE}, all values are included. Refer to the
#' \href{https://giscience.github.io/openrouteservice/documentation/extra-info/Extra-Info.html}{routing response documentation}
#' and the \href{https://openrouteservice.org/dev/#/ap-docs/v2/directions/{profile}/post}{API playground}
#' for more information on extra information.
#' @param elev_as_z \code{[logical]}
#' 
#' If \code{TRUE}, elevation data is stored as z-values in the
#' geometry of the output \code{sf} dataframe. If \code{FALSE}, elevation is
#' stored as a distinct dataframe column. Ignored if \code{elevation = FALSE}.
#' @inheritParams ors_distances
#' @returns \code{ors_inspect} returns an sf data.frame containing the smallest
#' possible linestrings for a route along with additional information on each
#' segment. \code{ors_summary} returns an object of type \code{route_summary}
#' that contains information on distances, durations, speed, elevation, detour
#' factors as well as all available extra information for the requested route.
#' @seealso \code{\link{ors_distances}},
#' \code{\link{plot_section}}
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' sample_source <- ors_sample(1)
#' sample_dest <- ors_sample(1)
#' profile = get_profiles()[1]
#' 
#' # Basic inspection without extra information
#' insp <- ors_inspect(sample_source, sample_dest, profile)
#' 
#' # Advanced inspection with extra information
#' insp_adv <- ors_inspect(
#'   sample_source,
#'   sample_dest,
#'   profile,
#'   extra_info = TRUE
#' )
#'                           
#' # Inspection of route elevation data
#' insp_elev <- ors_inspect(
#'   sample_source,
#'   sample_dest,
#'   profile,
#'   elevation = TRUE,
#'   elev_as_z = FALSE
#' )
#' 
#' # Inspection of route summary attributes
#' insp_attr <- ors_inspect(
#'   sample_source,
#'   sample_dest,
#'   profile,
#'   attributes = "detourfactor"
#' )
#' attr(insp_attr, "detourfactor")
#' 
#' # Altering the route by passing further arguments
#' insp_opts <- ors_inspect(
#'   sample_source,
#'   sample_dest,
#'   profile,
#'   continue_straight = TRUE,
#'   preference = "shortest",
#'   maximum_speed = 80
#' )
#' 
#' # Summarizing route specifics
#' route_summary <- ors_summary(sample_source, sample_dest, profile)
#' }
ors_inspect <- function(
  source,
  destination,
  profile = get_profiles(),
  attributes = list(),
  elevation = TRUE,
  extra_info = list(),
  instance = NULL,
  ...,
  elev_as_z = FALSE
) {
  if (is.null(instance)) {
    instance <- get_instance()
    
  }
  iid <- get_id(instance = instance)
  
  # Check if ORS is ready to use
  ors_ready(force = TRUE, error = TRUE, id = iid)

  # Bring input data into shape
  source <- format_input_data(source, to_coords = TRUE)
  destination <- format_input_data(destination, to_coords = TRUE)

  profile <- match.arg(profile)

  url <- get_ors_url(id = iid)

  features <- list(
    attributes = attributes,
    elevation = elevation,
    extra_info = extra_info
  )
  options <- format_ors_options(c(features, list(...)), profile)

  res <- query_ors_directions(
    source = source,
    destination = destination,
    profile = profile,
    units = "m",
    geometry = TRUE,
    options = options,
    url = url,
    token = instance$token
  )

  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

  geometry <- ors_multiple_linestrings(res, elev_as_z)
  elevation <- data.frame(elevation = attr(geometry, "elevation"))

  waypoints <- res$features$properties$segments[[1L]]$steps[[1L]]$way_points

  distances <- calculate_distances(geometry)
  durations <- calculate_durations(res, distances$distance)
  speeds <- calculate_avgspeed(distances$distance, durations$duration)

  if (is.element("avgspeed", options$attributes)) {
    avgspeed <- res$features$properties$segments[[1L]]$avgspeed
  } else avgspeed <- NULL

  if (is.element("detourfactor", options$attributes)) {
    detourfactor <- res$features$properties$segments[[1L]]$detourfactor
  } else detourfactor <- NULL

  names_wp <- res$features$properties$segments[[1L]]$steps[[1L]]$name
  expanded_names <- expand_by_waypoint(names_wp, waypoints)
  names <- gsub(pattern = "-", replacement = NA, expanded_names)
  names <- data.frame(names = names)

  ascent <- res$features$properties$segments[[1L]]$ascent
  descent <- res$features$properties$segments[[1L]]$descent

  extra_info <- vapply(
    options$extra_info,
    function(x) format_extra_info(res, x),
    data.frame(1L)
  )

  elements <- list(
    names,
    distances,
    durations,
    speeds,
    elevation,
    extra_info,
    geometry,
    stringsAsFactors = TRUE
  )

  elements <- elements[lengths(elements) != 0L]
  route <- do.call(data.frame, elements)

  route_sf <- structure(
    sf::st_as_sf(tibble::as_tibble(route)),
    avgspeed = avgspeed,
    detourfactor = detourfactor,
    ascent = ascent,
    descent = descent
  )
  route_sf
}


#' Plot route cross-sections
#' 
#' @description Plot the segments of a route as an elevation profile or
#' cross-section. A geographic cross-section describes a two-dimensional cut
#' through a route where the x-axis denotes the distance and the y-axis denotes
#' the elevation. The area below the elevation profile can be used to plot an
#' additional feature.
#' 
#' @param x \code{[sf]}
#' 
#' An \code{sf} data.frame describing segments of a linestring. The data.frame
#' is expected to have multiple rows (representing the segments) and at least
#' three columns, \code{"elevation"}, \code{"distance"} and an additional
#' feature. Preferably, this is a result of \code{ors_inspect}, but other
#' \code{sf} data.frames might work as well.
#' 
#' @param dist,elev,feat \code{[character]}
#' 
#' Column names of the distance, elevation and feature values inside \code{x}.
#' @param palette \code{[character]}
#' 
#' Color palette to be used for plotting. Passed on to
#' \code{\link[ggplot2]{scale_fill_manual}} or
#' \code{\link[ggplot2]{scale_fill_gradientn}}, depending on the data type of
#' \code{feat}. Defaults to the Cividis colormap.
#' @param scale_elevation \code{[logical]}
#' 
#' Whether to scale the elevation axis based on the lowest elevation. If
#' \code{FALSE}, the y-axis is not scaled and fixed at sea level. Defaults to
#' \code{TRUE}.
#' @param size \code{[numeric]}
#' 
#' Size of the line plot. Defaults to \code{1.5}.
#' @param xlab,ylab,scale_title,title,subtitle,caption \code{[character]}
#' 
#' Arguments to change label names, legend, title, subtitle and caption of the
#' ggplot object.
#' @param ... Further arguments passed to
#' \code{\link[ggplot2]{scale_fill_manual}} or
#' \code{\link[ggplot2]{scale_fill_gradientn}} depending on the data type of
#' \code{feat}.
#' 
#' @returns A \code{ggplot} object.
#' 
#' @examples
#' \dontrun{
#' sample <- ors_sample(2)
#' route <- ors_inspect(sample[1,], sample[2,], extra_info = TRUE)
#' 
#' plot_section(route, feat = "waytype")
#' 
#' plot_section(route, feat = "steepness", scale_elevation = FALSE)
#' 
#' zissou_pal <- hcl.colors(11, palette = "Zissou 1")
#' plot_section(route, "steepness", palette = zissou_pal)
#' 
#' terrain_pal <- hcl.colors(10, palette = "Terrain")
#' plot_section(route, "elevation", palette = terrain_pal, scale_elevation = FALSE)
#' }
#' 
#' @references 
#' \url{https://giscience.github.io/openrouteservice-r/articles/openrouteservice.html}
#' 
#' @export
plot_section <- function(
  x,
  feat = "avgspeed",
  dist = "distance",
  elev = "elevation",
  palette = NULL,
  scale_elevation = TRUE,
  size = 1.5,
  xlab = dist,
  ylab = elev,
  scale_title = feat,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  if (!requireNamespace("ggplot2")) {
    cli::cli_abort("the {.pkg ggplo2} package is necessary to create cross-sections.")
  }
  
  distance <- if (dist %in% names(x)) {
    x[[dist]]
  } else {
    cli::cli_abort("No distance data found in {.var x}.")
  }
  if (inherits(distance, "units")) {
    distance <- units::drop_units(units::set_units(distance, "km"))
  }
    
  elevation <- if (elev %in% names(x)) {
    x[[elev]]
  } else if (is_sf(x)) {
    sf::st_geometry(x)[, 3]
  } else {
    cli::cli_abort("No elevation data found in {.var x}.")
  }
  if (inherits(elevation, "units")) {
    elevation <- units::drop_units(units::set_units(elevation, "m"))
  }

  val <- if (feat %in% names(x)) {
    x[[feat]]
  } else {
    cli::cli_abort("No feature data found in {.var x}.")
  }
  if (inherits(val, "units")) {
    val <- units::drop_units(val)
  }
  
  n <- nrow(x)

  seg <- data.frame(
    xstart = cumsum(c(0, distance[-(n - 1)])),
    xend = cumsum(distance),
    ystart = elevation,
    yend = c(elevation[-1], elevation[length(elevation)])
  )
  y_range <- range(elevation) * c(0.9, 1.1)
  y_range[1] <- y_range[1] * scale_elevation

  ids <- seq(1, n)
  zip_x <- unlist(mapply(
    FUN = c,
    xmin1 = seg$xstart,
    xmax1 = seg$xend,
    xmax2 = seg$xend,
    xmin2 = seg$xstart,
    SIMPLIFY = FALSE
  ))
  
  zip_y <- unlist(mapply(
    FUN = c,
    ymin1 = rep(y_range[1], n),
    ymin2 = rep(y_range[1], n),
    ymax1 = seg$yend,
    ymay2 = seg$ystart,
    SIMPLIFY = FALSE
  ))
  
  poly <- data.frame(x = zip_x, y = zip_y, id = rep(ids, each = 4))

  val <- lapply(seq_along(val), function(i) if(is.na(val[i])) val[i - 1] else val[i])
  val <- data.frame(
    value = unlist(val),
    id = ids
  )
  
  poly <- merge(poly, val, by = "id")

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = seg,
      mapping = do.call(
        ggplot2::aes,
        lapply(list(x = "xstart", y = "ystart", xend = "xend", yend = "yend"), as.name)
      ),
      size = 1.5,
      lineend = "round",
      na.rm = TRUE
    ) +
    ggplot2::geom_polygon(
      data = poly,
      mapping = do.call(
        ggplot2::aes,
        lapply(list(x = "x", y = "y", group = "id", fill = "value"), as.name)
      ),
      na.rm = TRUE
    ) +
    units::scale_x_units(unit = "km", expand = c(0, 0)) +
    units::scale_y_units(unit = "m", expand = c(0, 0), limits = y_range) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggplot2::theme_bw()
  
  if (is.factor(val$value)) {
    if(is.null(palette)) {
      p <- p + ggplot2::scale_fill_viridis_d(option = "E", name = feat)
    } else {
      p <- p + 
        ggplot2::scale_fill_manual(
          values = palette,
          drop = FALSE,
          name = scale_title,
          ...
        )
    }
  } else {
    if (is.null(palette)) {
      p <- p + ggplot2::scale_fill_viridis_c(option = "E", name = feat)
    } else {
      p <- p +
        ggplot2::scale_fill_gradientn(
          colors = palette,
          name = scale_title,
          ...
        )
    }
  }
  
  p
}


#' @rdname ors_inspect
#' @export
ors_summary <- function(source, destination, profile = get_profiles(), instance = NULL, ...) {
  if (is.null(instance)) {
    instance <- get_instance()
  }
  iid <- get_id(instance = instance)
  
  # Check if ORS is ready to use
  ors_ready(force = TRUE, error = TRUE, id = iid)

  # Bring input data into shape
  source <- format_input_data(source, to_coords = TRUE)
  destination <- format_input_data(destination, to_coords = TRUE)

  profile <- match.arg(profile)

  url <- get_ors_url(id = iid)

  features <- list(attributes = TRUE, elevation = TRUE, extra_info = TRUE)
  options <- format_ors_options(append(features, list(...)), profile)

  res <- query_ors_directions(
    source = source,
    destination = destination,
    profile = profile,
    units = "m",
    geometry = TRUE,
    options = options,
    url = url,
    token = instance$token
  )

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
    summary <- extras[[info_type]]$summary[[1L]]
    summary$value <- fill_extra_info(summary$value, info_type, profile)
    summary
  }

  extras <- res$features$properties$extras
  summaries <- sapply(names(extras), get_ors_summaries, simplify = FALSE)

  summaries <- sapply(summaries, function(summary) {
    summary <- stats::aggregate(summary[, c(2L, 3L)],
                         by = summary["value"],
                         sum)
    row.names(summary) <- summary$value
    summary$value <- NULL
    summary
  }, simplify = FALSE)

  # Append summary tables and set units
  elev_speeds <- list(elevation = elevation_summary, avgspeed = speeds_summary)
  summaries <- append(summaries, elev_speeds, after = 0L)

  if (requireNamespace("units")) {
    summaries <- lapply(summaries, function(s) {
      s["distance"] <- units::as_units(s$distance, "m")
      s
    })
  }

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

  if (requireNamespace("units")) {
    units(output$distance) <- "m"
    units(output$duration) <- "s"
  }

  class(output) <- "route_summary"
  output
}
