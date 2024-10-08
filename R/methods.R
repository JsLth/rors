#' @export
format.ORSInstance <- function(x, ...) {
  # check if local or remote
  type <- ifelse(ors_is_local(x), "local", "remote")
  is_local <- identical(type, "local")
  ltype <- ifelse(is_local, paste0("(", get_ors_local_type(x), ")"), "")

  if (is.null(x$paths) && is_local) {
    return("<ORSInstance>")
  }

  active <- x$is_mounted()
  init <- if (is_local) x$is_init()
  server <- x$get_url()

  c(
    "<ORSInstance>",
    paste(" server :", server),
    paste(" type   :", type, ltype),
    paste(" active :", active),
    if (is_local) paste(" init   :", init)
  )
}

#' @export
format.ors_config <- function(x, ...) {
  # endpoints
  ep <- endpoints()
  sep <- x$parsed$ors$endpoints
  ep_fmt <- vapply(ep, FUN.VALUE = character(1), function(x) {
    enabled <- sep[[x]]$enabled %||% TRUE
    enabled_fmt <- ifelse(
      enabled,
      cli::col_green("enabled"),
      cli::col_red("disabled")
    )
    n_opts <- length(sep[[x]])
    sprintf("%s - {cli::no(%s)} option{?s} changed", enabled_fmt, n_opts)
  })
  names(ep_fmt) <- capitalize_char(paste0(ep, strrep("\u00a0", 11 - nchar(ep))))

  # profiles
  prof <- union(get_all_profiles(), names(x$profiles))
  prof_fmt <- prof %in% x$profiles | prof %in% names(x$profiles)
  prof_fmt <- ifelse(prof_fmt, cli::col_green(TRUE), cli::col_red(FALSE))
  prof <- paste0(prof, strrep("\u00a0", max(nchar(prof)) - nchar(prof) + 1))
  names(prof_fmt) <- prof
  cli::cli_format_method({
    cli::cli_h3("{.strong Endpoints}")
    cli::cli_dl(ep_fmt)
    cli::cli_h3("{.strong Profiles}")
    cli::cli_dl(prof_fmt)
  })
}


#' @export
format.ors_extract <- function(x, ...) {
  x$size <- sprintf("%s MB", x$size)
  names(x) <- paste(capitalize_char(names(x)), strrep(" ", 2))
  cli::cli_format_method({
    cli::cli_h3("{.strong Extract}")
    cli::cli_dl(unlist(x))
  })
}


#' @export
format.ors_settings <- function(x, ...) {
  memory <- round(unlist(x$memory), 2)
  memory[c("init", "max")] <- as.list(
    sprintf(
      "{.field %s}",
      unlist(memory[c("init", "max")])
    )
  )
  names(memory) <- c("Total", "Free", "Init", "Max")
  port_chr <- sprintf(
    paste(
      "{.field %s} {cli::symbol$arrow_right} %s (API),",
      "{.field %s} {cli::symbol$arrow_right} %s (Monitor)"
    ),
    x$ports[1L, 1L],
    x$ports[1L, 2L],
    x$ports[2L, 1L],
    x$ports[2L, 2L]
  )

  fmt <- c(
    Rebuild = "{.field {x$rebuild_graphs}}",
    Name = "{.field {x$name}}",
    Ports = port_chr,
    Version = "{.field {x$image}}",
    Memory = ""
  )
  wlen <- nchar(names(fmt))
  names(fmt) <- paste0(names(fmt), strrep("\u00a0", max(wlen) - wlen + 1))

  memory <- sprintf(
    "%s%s: %s",
    names(memory),
    strrep("\u00a0", 6 - nchar(names(memory))),
    sprintf("%s GB", memory)
  )

  cli::cli_format_method({
    cli::cli_h3("{.strong Settings}")
    cli::cli_dl(fmt)
    cli::cli_ul(memory)
  })
}


#' @export
format.ors_paths <- function(x, ...) {
  basedir <- basename2(x$top)
  compose <- basename2(x$compose)
  config <- basename2(x$config)
  extract <- basename2(x$extract)

  # get files in top directory
  subpaths <- dir(x$top)

  # get files in sub directories
  end_files <- lapply(seq_along(subpaths), function(i) {
    dir(file.path(x$top, subpaths[i]))
  })

  # append all files in relative top directories
  basedir <- cli::style_bold(basedir)
  top_dir <- c(base = basedir, top = subpaths, end = unlist(end_files))

  # append all children of a relative top directory
  apx <- rep(list(character()), length(unlist(end_files)))
  children <- c(base = list(subpaths), top = end_files, end = apx)

  # create colored arrow annotations
  annot <- top_dir
  for (file in c("compose", "config", "extract")) {
    fname <- get(file)
    i <- which(fname == top_dir)
    col <- switch(file, compose = "yellow", config = "blue", extract = "green")
    fun <- utils::getFromNamespace(sprintf("col_%s", col), ns = "cli")
    annot[i] <- paste(annot[i], fun(sprintf("<- %s", file)))
  }

  # merge file names to tree structure
  tree <- data.frame(
    stringsAsFactors = FALSE,
    dir = top_dir,
    children = I(children),
    annot = annot
  )

  cli::tree(tree, trim = TRUE, ...)
}


#' @export
format.ors_condition <- function(x, ...) {
  msg <- x$msg
  code <- x$code

  msg <- vapply(
    seq_along(msg),
    FUN.VALUE = character(1),
    function(i) {
      if (!is.null(code[i])) {
        type <- ifelse(x$error[i], "Error", "Warning")
        sprintf("%s code %s: %s", type, code[i], msg[i])
      } else {
        msg[i]
      }
    }
  )

  index <- seq_along(msg)
  max_nc <- nchar(max(index))
  msg <- lapply(index, function(i) {
    nc <- nchar(x$index[i])
    fmsg <- strwrap(
      msg[i],
      width = cli::console_width(),
      exdent = max_nc + 3,
      initial = paste0(x$index[i], strrep(" ", max_nc - nc), " - "),
      simplify = TRUE
    )
    if (length(fmsg) > 1) {
      fmsg <- paste(fmsg, collapse = "\n")
    }
    fmsg
  })

  title <- sprintf("Function: %s, timestamp: %s", x$call, x$ts)

  paste(
    cli::cli_format_method(cli::cli_h3(cli::style_bold(title)))[2],
    paste(msg, collapse = "\n"),
    sep = "\n"
  )
}


#' @export
format.ors_token <- function(x, ...) {
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

  if (is.null(msg2)) {
    paste0(c("", " "), c("<ors_token>", msg1))
  } else {
    paste0(c("", " ", " "), c("<ors_token>", msg1, msg2))
  }
}


#' @export
print.ors_condition <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}


#' @export
print.ors_condition_list <- function(x, ...) {
  for (cond in x) {
    print(cond, ...)
  }
  invisible(x)
}


#' @export
print.ORSInstance <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}


#' @export
print.ors_paths <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}


#' @export
print.ors_config <- function(x, ...) {
  cat(format(x, ...)[-1], sep = "\n")
  invisible(x)
}


#' @export
print.ors_extract <- function(x, ...) {
  cat(format(x, ...)[-1], sep = "\n")
  invisible(x)
}


#' @export
print.ors_settings <- function(x, ...) {
  cat(format(x, ...)[-1], sep = "\n")
  invisible(x)
}


#' @export
print.ors_local_sitrep <- function(x, ...) {
  for (obj in x) {
    print(obj)
    cli::cat_line()
  }
  invisible(x)
}


#' @export
print.ors_compose_parsed <- function(x, ...) {
  write_dockercompose(x, ...)
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
  invisible(x)
}


#' @export
print.ors_token <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}


#' @export
print.ors_params <- function(x, ...) {
  if (length(x)) {
    params <- cli::col_green(paste(cli::symbol$bullet, names(x)))
    names(params) <- rep(" ", length(params))

    msg <- cli::format_message(c(
      "{.cls ors_params} with the following parameters:",
      params
    ))
  } else {
    msg <- cli::format_message("Empty {.cls ors_params}")
  }

  cat(msg, "\n", ...)
  invisible(x)
}


#' @export
print.ors_geojson <- function(x, ...) {
  x$features <- "<truncated>"
  cat(jsonlite::toJSON(unclass(x), pretty = TRUE, auto_unbox = TRUE), ...)
  invisible(x)
}


#' @export
print.ors_logs <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}


#' @export
print.ors_status <- function(x, ...) {
  if ("languages" %in% names(x)) {
    x$languages <- paste(x$languages, collapse = ", ")
  }
  cat(yaml::as.yaml(drop_null(x)))
  invisible(x)
}
