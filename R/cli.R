#' cli_* distributor function that looks for a verbose variable in the parent
#' environment and terminates if the variable is FALSE
#' @param info,warn,progress,rule first argument passed to these cli functions
#' @param line logical indicating whether an empty line should be printed
#' @param ... further arguments passed to the respective functions
#' @noRd
ors_cli <- function(info = NULL,
                    bullets = NULL,
                    warn = NULL,
                    progress = NULL,
                    rule = NULL,
                    line = NULL,
                    ...,
                    .envir = NULL) {
  # TODO: make this more flexible
  .envir <- .envir %||% parent.frame()
  verbose <- get0("verbose", envir = parent.frame(), ifnotfound = 0L)

  if (!verbose) {
    private <- get0("private", envir = parent.frame(), ifnotfound = list())
    verbose <- private$.verbose %||% 0L
  }

  if (!verbose) {
    return(invisible(NULL))
  }

  if (!is.null(info)) {
    cli::cli_inform(info, ..., .envir = .envir)
  }

  if (!is.null(bullets)) {
    cli::cli_bullets(bullets, ..., .envir = .envir)
  }

  if (!is.null(warn)) {
    cli::cli_warn(warn, ..., .envir = parent.frame())
  }

  if (!is.null(progress)) {
    if (verbose > 1 && interactive()) {
      pfun <- get(
        paste0("cli_progress_", progress),
        envir = asNamespace("cli")
      )
      pfun(..., .envir = .envir)
    }
  }

  if (!is.null(rule)) {
    cli::cli_rule(rule, ..., .envir = parent.frame())
  }

  if (!is.null(line)) {
    cli::cat_line()
  }
}
