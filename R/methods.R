#' @export
print.ors_matrix <- function(x, ...) {
  prmatrix(x)
}


#' @export
print.route_summary <- function(x, ncols = 3L, table_gap = 2L, ...) {
  cli::cli_text("{.strong Attributes}")
  for (n in names(x[1:5])) {
    name <- capitalize_char(n)
    if (identical(n, "Detourfactor")) name <- "Detour factor"
    attribute_row <- paste(name, ": ", round(x[[n]], 2L), "\n")
    cat(attribute_row)
  }

  cat("\n")

  for (n in names(x[c(6L, 7L)])) {
    name <- capitalize_char(n)
    cli::cli_text("{.strong {name}}")
    print(x[[n]])
    cat("\n")
  }

  cli::cli_text("{.strong Additional infos}")

  tables <- x$tables

  # Get the maximum row length of the input tables
  row_lengths <- sapply(tables, function(tb) sapply(utils::capture.output(tb), nchar))
  max_row_length <- max(unlist(row_lengths))

  for (nc in seq(1, ncols - 1)) {
    if (cli::console_width() / ncols < 35) {
      ncols <- ncols - 1
    } else {
      break
    }
  }

  # Get an iterator that splits up the tables at the indices defined by ncols
  table_iterator <- split(tables, ceiling(seq_along(tables) / ncols))

  # Iterate through each table row (as defined by ncols)
  for (t in table_iterator) {
    # Number of rows of each table
    rows <- sapply(t, nrow)

    # Cumulated number of rows + table name + column names + empty line
    border_indices <- cumsum(rows + 3L)

    # Catch the output of print.data.frame and split it by table
    output <- utils::capture.output(t)
    output_split <- split(
      output,
      cumsum(is.element(
        seq_along(output),
        border_indices + 1L
      ))
    )

    # Get the maximum number of rows of one table row
    max_length <- max(sapply(output_split, length))

    # Iterate through each table of one table row
    op_vec <- lapply(output_split, function(op_vec) {
      # Remove empty character string
      op_vec <- op_vec[nchar(op_vec) != 0L]

      # If the table name is shorter than the row length, fill with white space
      add_ws <- function(char, row_length, max_length) {
        if (row_length < max_length) {
          paste0(char, strrep(" ", max_length - row_length))
        } else {
          char
        }
      }

      name <- op_vec[1L]
      name <- add_ws(name, nchar(name), max_row_length)

      columns <- op_vec[2L]
      row_length <- nchar(columns)
      columns <- add_ws(columns, row_length, max_row_length)

      rows <- op_vec[seq(3, length(op_vec))]
      rows <- unlist(lapply(rows, add_ws, row_length, max_row_length))

      # Get the number of rows. If the table has less rows than the table
      # with the most rows, add rows and fill them with white space
      len <- length(rows)
      if (len < max_length) {
        rows[seq(len + 1L, max_length - 2L)] <- strrep(" ", max_row_length)
      }
      c(name, columns, rows)
    })

    # Transpose list to align corresponding rows from each table
    to_vector <- lapply(do.call(Map, c(f = c, op_vec)), unlist)
    # Piece together character strings
    to_string <- lapply(
      to_vector,
      function(v) {
        if (nchar(paste(v, collapse = "")) + table_gap >= cli::console_width()) {
          table_gap <- 0
        }
        trimws(paste(v, collapse = strrep(" ", table_gap)), which = "right")
      }
    )
    to_string <- lapply(to_string, trimws, which = "right")
    row_string <- paste(to_string, collapse = "\n")
    cat(row_string, "\n")
  }

  invisible(x)
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
        } else {
          fmsg
        }
      })

      printed_time <- paste0("Function call from ", time, ":")
      paste(printed_time, paste(messages, collapse = "\n"), sep = "\n")
    }
  })

  if (!is.null(calls)) {
    calls <- Filter(is.character, calls)
    cat(paste0(paste(calls, collapse = "\n\n"), "\n"))
  }

  invisible(x)
}


#' @export
print.ors_instance <- function(x, ...) {
  if (any_mounted()) {
    mounted <- get_instance()
    active <- if (!is.null(x$url)) {
      identical(x$url, mounted$url)
    } else {
      "compose" %in% names(x) && identical(x$paths$dir, mounted$paths$dir)
    }
  } else {
    active <- FALSE
  }

  alive <- attr(x, "alive")
  built <- ors_built(x$paths$dir)
  type <- attr(x, "type")

  if (identical(type, "remote")) {
    server <- x$url
  } else {
    server <- paste0("https://localhost:", x$compose$ports$host[1], "/")
  }
  
  cat(
    "<ors_instance>", "\n",
    " server :", server, "\n",
    " active :", active, "\n",
    " alive  :", alive, "\n",
    if (identical(type, "local")) c(" built  :", built, "\n"),
    " type   :", type, "\n"
  )
  invisible(x)
}


#' @export
print.ors_instance_paths <- function(x, ...) {
  basedir <- relative_path(x$dir, path.expand("~"))
  compose <- if (!is.null(x$compose_path)) basename(x$compose_path) else NULL
  config <- if (!is.null(x$config_path)) basename(x$config_path) else NULL
  extract <- if (!is.null(x$extract_path)) basename(x$extract_path) else NULL

  config_dir <- ifelse(grepl("\\/conf\\/", x$config_path), "conf", "data")
  docker_path <- file.path(x$dir, "docker")
  subdocker_paths <- file.path(docker_path, dir(docker_path))
  subdocker_paths_files <- lapply(seq_along(subdocker_paths), function(i) {
    d <- dir(subdocker_paths[i])
    if (basename(subdocker_paths[i]) != config_dir && "ors-config.json" %in% d) {
      ci <- which("ors-config.json" == d)
      d[ci] <- paste0(d[ci], " ")
    }
    d
  })

  if (compose %in% basename(subdocker_paths)) {
    compose_i <- which(compose == basename(subdocker_paths)) + 2
  }
  if (!is.null(config) && config %in% unlist(subdocker_paths_files)) {
    config_i <- which(config == unlist(subdocker_paths_files)) + 2 + length(subdocker_paths)
  } else {
    config_i <- NULL
  }
  if (!is.null(extract) && config %in% unlist(subdocker_paths_files)) {
    extract_i <- which(extract == unlist(subdocker_paths_files)) + 2 + length(subdocker_paths)
  } else {
    extract_i <- NULL
  }

  top_dir <- c(
    basedir,
    "docker",
    basename(file.path(docker_path, dir(docker_path))),
    unlist(subdocker_paths_files)
  )

  children <- c(
    list("docker"),
    list(basename(file.path(docker_path, dir(docker_path)))),
    subdocker_paths_files,
    lapply(seq(1, length(unlist(subdocker_paths_files))), function(i) character(0))
  )

  annot <- top_dir
  annot[compose_i] <- paste(annot[compose_i], "\033[33m<- compose file\033[39m")
  annot[config_i] <- paste(annot[config_i], "\033[34m<- config file\033[39m")
  annot[extract_i] <- paste(annot[extract_i], "\033[32m<- extract file\033[39m")

  tree <- cli::tree(
    data.frame(
      stringsAsFactors = FALSE,
      dir = top_dir,
      children = I(children),
      annot = annot
    ),
    ...
  )

  tree <- gsub(" ", "\u00a0", tree)

  for (node in tree) {
    cli::cli_text(node)
  }

  invisible(x)
}


#' @export
print.ors_instance_extract <- function(x, ...) {
  cat("Name :", x$name, "\n")
  cat("Size :", x$size, "MB")
  invisible(x)
}


#' @export
print.ors_instance_config <- function(x, ...) {
  allp <- get_all_profiles(x$parsed)
  allp_in <- allp %in% x$profiles
  allp_in <- lapply(allp_in, ifelse, cli::col_green(T), cli::col_red(F))

  allp <- sapply(allp, function(p) {
    paste0(p, strrep("\u00a0", 14L - nchar(p)))
  })

  names(allp_in) <- allp

  cli::cli_dl(do.call(c, allp_in))
  invisible(x)
}


#' @export
print.ors_instance_settings <- function(x, ...) {
  names(x$memory) <- c("Total memory", "Free memory", "Initial memory", "Max memory")
  mem_list <- lapply(x$memory, function(m) paste(round(m, 2), "GB"))
  mem_df <- as.data.frame(t(mem_list))
  port_chr <- sprintf(
    "%s -> %s, %s -> %s",
    x$ports[1L, 1L],
    x$ports[1L, 2L],
    x$ports[2L, 1L],
    x$ports[2L, 2L]
  )

  prot <- ifelse(x$auto_deletion, "unprotected", "protected")
  
  cli::cli_text("Mode\u00a0\u00a0\u00a0: {x$graph_building}")
  cli::cli_text("Name\u00a0\u00a0\u00a0: {x$name}")
  cli::cli_text("Ports\u00a0\u00a0: {port_chr}")
  cli::cli_text("Config\u00a0: {prot}")
  cli::cat_line()
  print(mem_df, right = FALSE, row.names = FALSE)
  invisible(x)
}


#' @export
print.ors_instance_status <- function(x, ...) {
  gln <- c(
    "Docker running",
    "Image exists",
    "Container built",
    "Container running",
    "Service ready"
  )

  gl <- sapply(x, ifelse, cli::col_green(TRUE), cli::col_red(FALSE))

  gln <- sapply(gln, function(n) {
    paste0(n, strrep("\u00a0", 18L - nchar(n)))
  })

  names(gl) <- gln

  cli::cli_dl(gl)
  invisible(x)
}


#' @export
print.ors_compose <- function(x, ...) {
  cat(yaml::as.yaml(x, indent.mapping.sequence = TRUE), "\n")
  invisible(x)
}


#' @export
print.ors_config <- function(x, ...) {
  utils::str(x, max.level = 3, give.attr = FALSE)
  invisible(x)
}
