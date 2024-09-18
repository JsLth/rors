#' cli_* distributor function that looks for a verbose variable in the parent
#' environment and terminates if the variable is FALSE
#' @param ... Key-value pairs where the key is the name to a cli_* function
#' and the values are the arguments. In case the key is "cat", invokes
#' \code{cli::cat_line}. If \code{cat = "line"}, calls \code{cli::cat_line()}.
#' If key is "progress", first value needs to be the name of the progress
#' type (bar, step, update, done). "info" is short-hand for cli::cli_inform.
#' @param .envir Environment from which to collect the "verbose" symbol and
#' in which to call cli_* functions that require an .envir argument.
#' @noRd
ors_cli <- function(..., .envir = parent.frame()) {
  verbose <- get0("verbose", envir = .envir, ifnotfound = FALSE)

  if (!verbose) {
    private <- get0("private", envir = .envir, ifnotfound = list())
    verbose <- private$.verbose %||% FALSE
  }

  if (!verbose) {
    return(invisible(NULL))
  }

  dots <- list(...)
  for (i in seq_len(...length())) {
    cfun <- names(dots[i])
    args <- as.list(dots[[i]])

    # dispatch cat arguments to cli::cat_line
    if (identical(cfun, "cat")) {
      # replace "line" with line breaks
      line <- which(args %in% "line")
      args[line] <- rep("\n", length(line))
      do.call(cli::cli_verbatim, args)
      next
    }

    # handle progress bars
    if (identical(cfun, "progress")) {
      cfun <- sprintf("%s_%s", cfun, args[[1]])
      args[[1]] <- NULL
    }

    # info is short for inform
    if (identical(cfun, "info")) {
      cfun <- "inform"
    }

    cfun <- get(sprintf("cli_%s", cfun), envir = asNamespace("cli"))

    # pass .envir only when it is needed
    if (".envir" %in% names(formals(cfun))) {
      args <- c(args, .envir = .envir)
    }

    do.call(cfun, args)
  }
}


escape_cli <- function(x) {
  gsub("\\}", "}}", gsub("\\{", "{{", x))
}


cli_once <- function(name, msg, verbose) {
  if (!isTRUE(get0(name, envir = ors_cache))) {
    ors_cli(info = list(msg))
    assign(name, TRUE, envir = ors_cache)
  }
}
