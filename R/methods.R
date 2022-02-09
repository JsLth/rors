#' @export

print.ORSExtract <- function(x, ...) {
  if (is.null(x$path)) {
    extract_file <- NA_character_
  } else {
    extract_file <- basename(x$path)
  }
  
  if (is.null(x$size)) {
    extract_size <- NA_character_
  } else {
    extract_size <- paste(x$size, "MB")
  }
  
  cli::cli_text("Class\u00a0\u00a0\u00a0: {.cls {class(x)}}")
  cli::cli_text("Path\u00a0\u00a0\u00a0\u00a0: {x$dir}")
  cli::cli_text("Extract\u00a0: {extract_file}")
  cli::cli_text("Size\u00a0\u00a0\u00a0\u00a0: {extract_size}")
  cat("\n")
  cli::cli_text("Public methods:")
  print(names(ORSExtract$public_methods), ...)
}


#' @export

print.ORSConfig <- function(x, ...) {
  if (grepl("docker/data", x$path, fixed = TRUE)) {
    pd <- "pre-setup"
  } else if (grepl("docker/conf", x$path, fixed = TRUE)) {
    pd <- "post-setup"
  } else {
    pd <- NULL
  }
  
  allp <- c("car", "hgv", "bike-regular", "bike-mountain", "bike-road",
            "bike-electric", "walking", "hiking", "wheelchair")
  allp_in <- allp %in% x$active_profiles
  allp_in <- lapply(allp_in, ifelse, cli::col_green(T), cli::col_red(F))
  
  allp <- sapply(allp, function(p) {
    paste0(p, strrep("\u00a0", 14 - nchar(p)))
  })
  
  names(allp_in) <- allp
  
  cli::cli_text("Class\u00a0\u00a0: {.cls {class(x)}}")
  cli::cli_text("Path\u00a0\u00a0\u00a0: {x$dir}")
  cli::cli_text("Status\u00a0: {pd}")
  cat("\n")
  cli::cli_dl(do.call(c, allp_in))
  cat("\n")
  cli::cli_text("Public methods:")
  print(names(ORSConfig$public_methods), ...)
}


#' @export

print.ORSSetupSettings <- function(x, ...) {
  names(x$memory) <- c("Total memory", "Free memory", "Initial memory", "Max memory")
  mem_list <- lapply(x$memory, function(m) paste(round(m, 2), "GB"))
  mem_df <- as.data.frame(t(mem_list))
  port_chr <- sprintf(
    "%s -> %s, %s -> %s",
    x$ors_ports[1, 1],
    x$ors_ports[1, 2],
    x$ors_ports[2, 1],
    x$ors_ports[2, 2]
  )
  
  cli::cli_text("Class\u00a0: {.cls {class(x)}}")
  cli::cli_text("Path\u00a0\u00a0: {x$dir}")
  cli::cli_text("Mode\u00a0\u00a0: {x$graph_building}")
  cli::cli_text("Name\u00a0\u00a0: {x$ors_name}")
  cli::cli_text("Ports\u00a0: {port_chr}")
  cat("\n")
  print(mem_df, right = FALSE, row.names = FALSE)
  cat("\n")
  cli::cli_text("Public methods:")
  print(names(ORSSetupSettings$public_methods), ...)
}


#' @export

print.ORSDockerInterface <- function(x, ...) {
  gl <- list(
    x$docker_running,
    x$image_exists,
    x$container_built,
    x$container_running,
    x$service_ready
  )
  
  gln <- c(
    "Docker running",
    "Image exists",
    "Container built",
    "Container running",
    "Service ready"
  )
  
  gl <- sapply(gl, ifelse, cli::col_green(TRUE), cli::col_red(FALSE))
  
  gln <- sapply(gln, function(n) {
    paste0(n, strrep("\u00a0", 18 - nchar(n)))
  })
  
  names(gl) <- gln
  
  cli::cli_text("Class\u00a0: {.cls {class(x)}}")
  cli::cli_text("Path\u00a0\u00a0: {x$dir}")
  cat("\n")
  cli::cli_dl(gl)
  cat("\n")
  cli::cli_text("Public methods:")
  print(names(ORSDockerInterface$public_methods))
}


#' @export

print.ors_matrix <- function(x, ...) {
  prmatrix(x)
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
  row_lengths <- sapply(tables, function(tb) sapply(utils::capture.output(tb), nchar))
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
    output <- utils::capture.output(t)
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
    to_vector <- lapply(do.call(Map, c(f = c, op_vec)), unlist)
    # Piece together character strings
    to_string <- lapply(
      to_vector,
      function(v) trimws(paste(v, collapse = strrep(" ", 8)), which = "right")
    )
    to_string <- lapply(to_string, trimws, which = "right")
    row_string <- paste(to_string, collapse = "\n")
    cat(row_string)
    cat("\n")
  }
}


#' @export

print.ors_condition <- function(x, ...) {
  timestamps <- names(x)
  
  calls <- sapply(timestamps, function(time) {
    if (length(x[[time]]$conditions)) {
      indices <- attr(x[[time]], "row.names")
      messages <- x[[time]]$conditions
      max_nc <- nchar(max(indices))
      messages <- lapply(seq(1, length(messages)), function(mi) {
        nc <- nchar(indices[mi])
        fmsg <- strwrap(
          messages[[mi]],
          exdent = nc + 3,
          initial = paste0(indices[mi], strrep(" ", max_nc - nc), " - "),
          simplify = TRUE
        )
        if (length(fmsg) > 1) {
          paste(fmsg, collapse = "\n")
        } else fmsg
      })
      
      printed_time <- paste0("Function call from ", time, ":")
      paste(printed_time, paste(messages, collapse = "\n"), sep = "\n")
    }
  })

  if (!is.null(calls)) {
    calls <- Filter(is.character, calls)
    cat(paste0(paste(calls, collapse = "\n\n"), "\n"))
  } else {
    invisible()
  }
  
}
