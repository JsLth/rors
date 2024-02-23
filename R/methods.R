#' @export
print.ors_matrix <- function(x, ...) {
  print(structure(x, class = NULL))
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
print.ORSInstance <- function(x, ...) {
  # check if local or remote
  type <- ifelse(ors_is_local(x), "local", "remote")
  is_local <- identical(type, "local")

  if (is.null(x$paths) && is_local) {
    cat("<ORSInstance>", "\n")
    return(invisible(x))
  }

  # check if instance is mounted to the session
  active <- x$is_mounted()

  # check if instance on initial run
  init <- switch(
    type,
    local = dir.exists(file.path(x$paths$top, "docker", "graphs")),
    remote = NULL
  )

  # find server for requests
  server <- switch(
    type,
    local = sprintf("https://localhost:%s/", x$compose$ports[1, 1]),
    remote = get_ors_url(x$server)
  )

  cat(
    "<ORSInstance>", "\n",
    " server :", server, "\n",
    " type   :", type, "\n",
    " active :", active, "\n",
    if (is_local) paste(" init   :", init, "\n")
  )

  invisible(x)
}


#' @export
print.ors_paths <- function(x, ...) {
  basedir <- basename(x$top)
  compose <- if (!is.null(x$compose)) basename(x$compose) else NULL
  config <- if (!is.null(x$config)) basename(x$config) else NULL
  extract <- if (!is.null(x$extract)) basename(x$extract) else NULL

  config_dir <- ifelse(grepl("\\/conf\\/", x$config), "conf", "data")
  docker_path <- file.path(x$top, "docker")
  subdocker_paths <- file.path(docker_path, dir(docker_path))
  subdocker_paths_files <- lapply(seq_along(subdocker_paths), function(i) {
    d <- dir(subdocker_paths[i])
    if (basename(subdocker_paths[i]) != config_dir && "ors-config.json" %in% d) {
      ci <- which("ors-config.json" == d)
      d[ci] <- paste0(d[ci], " ")
    }
    d
  })

  topdir_files <- list.files(x$top, full.names = TRUE)
  docker_files <- list.files(docker_path, full.names = TRUE)
  end_files <- subdocker_paths_files[lengths(subdocker_paths_files) > 0]

  top_dir <- c(
    base = basedir,
    top = basename(topdir_files),
    sub = basename(file.path(docker_path, dir(docker_path))),
    end = unlist(end_files)
  )

  children <- c(
    base = list(dir(x$top)),
    top = lapply(topdir_files, \(x) list.files(x)),
    sub = subdocker_paths_files,
    end = rep(list(character()), length(unlist(end_files)))
  )

  compose_i <- config_i <- extract_i <- NULL
  if (!is.null(compose) && compose %in% top_dir) {
    compose_i <- which(compose == top_dir)
  }

  if (!is.null(config) && config %in% top_dir) {
    config_i <- which(config == top_dir)
  }

  if (!is.null(extract) && extract %in% top_dir) {
    extract_i <- which(extract == top_dir)
  }

  annot <- top_dir
  annot[compose_i] <- paste(annot[compose_i], "\033[33m<- compose\033[39m")
  annot[config_i] <- paste(annot[config_i], "\033[34m<- config\033[39m")
  annot[extract_i] <- paste(annot[extract_i], "\033[32m<- extract\033[39m")

  tree <- cli::tree(
    data.frame(
      stringsAsFactors = FALSE,
      dir = top_dir,
      children = I(children),
      annot = annot
    ),
    trim = TRUE,
    ...
  )

  cat(tree, sep = "\n")
  invisible(x)
}


#' @export
print.ors_config <- function(x, ...) {
  allp <- union(get_all_profiles(), names(x$profiles))
  allp_in <- allp %in% x$profiles | allp %in% names(x$profiles)
  allp_in <- lapply(allp_in, ifelse, cli::col_green(TRUE), cli::col_red(FALSE))

  allp <- sapply(allp, function(p) {
    paste0(p, strrep("\u00a0", max(nchar(allp)) - nchar(p) + 1))
  })

  names(allp_in) <- allp

  cli::cli_dl(do.call(c, allp_in))
  invisible(x)
}


#' @export
print.ors_settings <- function(x, ...) {
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

  cli::cli_text("Build\u00a0\u00a0: {x$graph_building}")
  cli::cli_text("Name\u00a0\u00a0\u00a0: {x$name}")
  cli::cli_text("Ports\u00a0\u00a0: {port_chr}")
  cli::cli_text("Image\u00a0\u00a0: {x$image}")
  cli::cat_line()
  print(mem_df, right = FALSE, row.names = FALSE)
  invisible(x)
}


#' @export
print.ors_compose_parsed <- function(x, ...) {
  cat(yaml::as.yaml(x, indent.mapping.sequence = TRUE), "\n")
  invisible(x)
}


#' @export
print.ors_config_parsed <- function(x, ...) {
  utils::str(x, max.level = 3, give.attr = FALSE)
  invisible(x)
}


#' @export
print.ors_profile <- function(x, ...) {
  cat("<ors_profile>\n", write_config(x, ...))
}


#' @export
print.stprof <- function(x, ...) {
  cat(write_config(x, ...))
}


#' @export
print.ors_token <- function(x, ...) {
  active <- attr(x, "active")

  if (active) {
    emph <- cli::style_underline("requires")
    msg1 <- cli::format_message(paste(
      "This instance", emph, "a token."
    ))

    if (x) {
      msg2 <- cli::format_message(c(
        "v" = "A token is stored in the `ORS_TOKEN` environment variable."
      ))
    } else {
      msg2 <- cli::format_message(c(
        "x" = "No token found in the `ORS_TOKEN` environment variable."
      ))
    }
  } else {
    emph <- cli::style_underline("no")
    msg1 <- cli::format_message(paste(
      "This instance requires", emph, "token."
    ))

    msg2 <- NULL
  }




  cat("<ors_token>", "\n", msg1, "\n", msg2, if (!is.null(msg2)) "\n")
}
