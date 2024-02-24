#' cli_* distributor function that looks for a verbose variable in the parent
#' environment and terminates if the variable is FALSE
#' @param info,warn,progress,rule first argument passed to these cli functions
#' @param line logical indicating whether an empty line should be printed
#' @param ... further arguments passed to the respective functions
#' @noRd
ors_cli <- function(..., .envir = NULL) {
  .envir <- .envir %||% parent.frame()
  verbose <- get0("verbose", envir = .envir, ifnotfound = 0L)

  if (!verbose) {
    private <- get0("private", envir = .envir, ifnotfound = list())
    verbose <- private$.verbose %||% 0L
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
      args[line] <- list(NULL)
      do.call(cli::cat_line, args)
      next
    }

    # handle progress bars
    if (identical(cfun, "progress")) {
      if (verbose < 2 || !interactive()) next
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
