"%||%" <- function(x, y) {
  if (is.null(x)) y else x
}


"%NA%" <- function(x, y) {
  if (all(is.na(x))) y else x
}


"%empty%" <- function(x, y) {
  if (!length(x) == 0) x else y
}


last <- function(x) {
  if (length(x)) {
    x[[length(x)]]
  }
}


drop_null <- function(x) {
  x[lengths(x) > 0]
}


drop_na <- function(x) {
  x[!is.na(x)]
}


vswitch <- function(object, ..., FUN.VALUE = character(1), USE.NAMES = FALSE) {
  vapply(object, switch, FUN.VALUE = FUN.VALUE, USE.NAMES = USE.NAMES, ...)
}


#' Checks if a URL is local (i.e. localhost or 127.0.0.1)
#' @noRd
is_local <- function(url) {
  grepl("^(https?:\\/\\/)?[[:alnum:]\\.]+\\:[[:digit:]]+\\/?", url, perl = TRUE)
}


#' Checks if a character string is a valid URL
#' @noRd
is_url <- function(url) {
  grepl(
    "^(https?:\\/\\/)?[[:alnum:]\\.]+(\\.[[:lower:]]+)|(:[[:digit:]])\\/?",
    url,
    perl = TRUE
  )
}


#' Checks if a URL leads to the public API of OpenRouteService (api.openrouteservice)
#' @noRd
is_ors_api <- function(url) {
  grepl(substr(public_api, 9, 32), url, fixed = TRUE)
}


#' Checks if a string is a numeric version. It is if a call to numeric_version
#' does not return NA
#' @noRd
is_numver <- function(x) {
  !is.na(numeric_version(x, strict = FALSE))
}


is_version_desc <- function(x, platform = NULL) {
  gh <- "master"
  dh <- c("latest", "nightly")
  if (identical(platform, "gh")) {
    x %in% gh
  } else if (identical(platform, "dh")) {
    x %in% dh
  } else {
    x %in% c(gh, dh)
  }
}


minimum_version <- function(v1, v2) {
  if (is_numver(v1) && is_numver(v2)) {
    numeric_version(v1) >= numeric_version(v2)
  }
}


timestamp <- function() {
  format(Sys.time(), format = "%H:%M:%OS6")
}


loadable <- function(pkg) {
  suppressPackageStartupMessages(requireNamespace(pkg, quietly = TRUE))
}


#' Given two absolute paths, constructs a relative path
#' @param targetdir Path that should be turned to a relative path
#' @param basedir Path that contains `targetdir`
#' @param pretty Whether to add a tilde and a slash in front
#' @noRd
relative_path <- function(targetdir, basedir, pretty = FALSE) {
  rel <- gsub(
    pattern = sprintf("%s/?", basedir),
    replacement = "",
    x = targetdir
  )

  if (!nzchar(rel)) {
    rel <- "."
  } else if (pretty) {
    rel <- file.path(".", rel)
  }

  rel
}


#' Given a path, returns the parent path
#' @param ndir Number of times to up a folder
#' @noRd
file_path_up <- function(path, ndir = 1L) {
  for (i in seq_len(ndir)) {
    path <- dirname(path)
  }
  path
}


#' basename but returns NULL, if input is NULL
#' @noRd
basename2 <- function(x) {
  basename(x %||% NA) %NA% NULL
}


#' Capitalizes the first symbol of a character string
#' @noRd
capitalize_char <- function(string) {
  cap_string <- tolower(as.character(string))
  substr(cap_string, 1L, 1L) <- toupper(substr(string, 1L, 1L))
  cap_string
}


#' Simple wrapper around regexec and regmatches to find a regex pattern in a text
#' @noRd
regex_match <- function(text, pattern, ...) {
  regmatches(text, regexec(pattern, text, ...))
}


#' Creates a count table of x
#' @noRd
count <- function(x) {
  df <- stats::aggregate(x, list(x), length)
  names(df) <- c("level", "count")
  df[order(df$count, decreasing = TRUE), ]
}


#' Binds list of (sf) data.frames to a single data.frame. If the number of
#' columns differs, fills empty columns with NA
#' @param args List of data.frames or sf objects
#' @returns data.frame or sf data.frame
#' @noRd
rbind_list <- function(args) {
  nam <- lapply(args, names)
  unam <- unique(unlist(nam))
  len <- vapply(args, length, numeric(1))
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    if (nrow(args[[i]])) {
      nam_diff <- setdiff(unam, nam[[i]])
      if (length(nam_diff)) {
        args[[i]][nam_diff] <- NA
      }
    } else {
      next
    }
  }
  out <- do.call(rbind, args)
  rownames(out) <- NULL
  out
}


#' Modified version of utils::modifyList that can handle unnamed lists
#' @returns x, modified according to y
#' @noRd
modify_list <- function(x, y) {
  idx <- names(y) %||% rev(seq_along(y))

  for (i in idx) {
    x_i <- x[[i]]
    y_i <- y[[i]]

    if (is.list(x_i) && is.list(y_i)) {
      x[[i]] <- modify_list(x_i, y_i)
    } else {
      x[[i]] <- y_i
    }
  }
  x
}


#' Flattens a nested list and keeps the list structure
#' @param x nested list
#' @returns list containing each nested list at the top level
#' @noRd
flatten_list <- function(x) {
  # https://stackoverflow.com/a/8142955
  while (any(vapply(x, is.list, logical(1)))) {
    x <- lapply(x, function(x) if (is.list(x)) x else list(x))
    x <- unlist(x, recursive = FALSE)
  }
  x
}


#' Checks if list x is modifiable by list y, i.e. if modify_list would
#' change something. NULL is ignored.
#' @param x,y Named lists
#' @noRd
equivalent_list <- function(x, y) {
  y <- y[!vapply(y, is.null, logical(1))]
  idx <- names(y) %||% rev(seq_along(y))
  eqv <- TRUE
  for (i in idx) {
    # check if y introduces new names
    eqv <- eqv && all(names(y[[i]] %in% names(x[[i]])))
    if (is.list(x[[i]]) && is.list(y[[i]])) {
      eqv <- eqv && equivalent_list(x[[i]], y[[i]])
    } else {
      eqv <- eqv && isTRUE(all.equal(x[[i]], y[[i]]))
    }

  }
  eqv
}


is_list <- function(x) {
  inherits(x, "list")
}


#' Converts a decimal to its binary representation, e.g. 3 -> c(2, 1)
#' @returns Numeric vector of varying length
#' @noRd
decode_base2 <- function(code) {
  is_base2 <- log2(code) %% 1L == 0L || code == 0L
  if (is_base2) {
    return(code)
  }
  base2_vector <- 0L
  i <- 0L
  while (utils::tail(base2_vector, 1L) < code) {
    base2_vector[i + 1L] <- 2L^i
    i <- i + 1L
  }
  base2_vector <- rev(utils::head(base2_vector, -1L))
  for (b in seq(1L, length(base2_vector))) {
    if (b > 1L) {
      rbase2 <- utils::tail(base2_vector, -(b - 1L))
    } else {
      rbase2 <- base2_vector
    }
    res <- NULL
    for (ni in seq(1L, length(rbase2))) {
      num <- rbase2[ni]
      code_sum <- num + sum(res[!is.na(res)])
      if (code_sum <= code) {
        res[ni] <- num
      }
      if (code_sum == code) {
        return(res[!is.na(res)])
      }
    }
  }
}


#' Turns a vector to a single-element list
#' @noRd
box <- function(x) {
  if (length(x) == 1 && is.atomic(x)) {
    x <- list(x)
  }
  x
}


#' Turns input data.frames to a single data.frame without seperating the columns.
#' Comparable to tidyr::nest, just with base R
#' @param ... Data.frames where the argument name is the column name of the
#' entire data.frame within the output data.frame and the names are the column
#' names within the data.frame that should be nested.
#' @returns Nested data.frame where each nested data.frame can be accessed using
#' their argument in `...`.
#' @noRd
df_nest <- function(...) {
  x <- list(...)
  class(x) <- "data.frame"
  attr(x, "row.names") <- seq_len(nrow(x[[1]]))
  x
}


as_data_frame <- function(...) { # nocov start
  if (loadable("tibble")) {
    tibble::as_tibble(...)
  } else {
    as.data.frame(...)
  }
}


data_frame <- function(...) {
  if (loadable("tibble")) {
    tibble::tibble(...)
  } else {
    data.frame(...)
  }
} # nocov end


#' Simple yes-no input that defines the output of yes/no answers.
#'
#' @param msg Message to show in the input field
#' @param yes,no What to return in case yes/no is selected
#' @param dflt What to return in case the session is not interactive
#' @param ask Whether to show a prompt or not. If FALSE, returns dflt
#' @returns Arguments yes or no
#' @noRd
yes_no <- function(msg, yes = TRUE, no = FALSE, dflt = NULL, ask = TRUE) { # nocov start
  if (!interactive() || !ask) {
    return(dflt)
  }

  input <- readline(paste0(msg, " (y/N/Cancel) "))

  # If neither yes or no is given as input, try again
  if (!input %in% c("y", "N", "Cancel")) {
    Recall(msg, yes = yes, no = no)
  }

  switch(input, y = yes, N = no, Cancel = cancel())
}


#' Safely cancel a function without invoking an error
#' @param msg What to cancel, e.g. a function or a selection
#' @noRd
cancel <- function(msg = "Input interrupted.") {
  cli::cli_inform(c("x" = msg))
  invokeRestart("abort")
} # nocov end


#' Wrapper around cli::cli_abort()
#' @noRd
abort <- function(msg, class = NULL, .envir = parent.frame(), ...) {
  class <- paste0("ors_", class %||% "error")
  cli::cli_abort(msg, class = class, .envir = .envir, ...)
}


#' Returns an object from a specified environment
#' @noRd
return_from_parent <- function(obj, .envir = parent.frame()) {
  do.call("return", args = box(obj), envir = .envir)
}
