#' cli_* distributor function that looks for a verbose variable in the parent
#' environment and terminates if the variable is FALSE
#' @param info,warn,progress,rule first argument passed to these cli functions
#' @param line logical indicating whether an empty line should be printed
#' @param ... further arguments passed to the respective functions
#' @noRd
ors_cli <- function(info = NULL,
                    warn = NULL,
                    progress = NULL,
                    rule = NULL,
                    line = NULL,
                    ...) {
  args <- as.list(environment())
  verbose <- get0("verbose", envir = parent.frame(), ifnotfound = FALSE)
  
  if (!verbose) {
    return(invisible(NULL))
  }
  
  if (sum(!vapply(args, is.null, logical(1))) > 1) {
    cli::cli_abort("Can only handle one cli element at a time.")
  }

  if (!is.null(info)) {
    cli::cli_inform(info, ...)
  }

  if (!is.null(warn)) {
    cli::cli_warn(warn, ...)
  }

  if (!is.null(progress)) {
    verbose <- verbose && interactive()
    pfun <- get(
      paste0("cli_progress_", progress),
      envir = asNamespace("cli")
    )
    pfun(...)
  }
  
  if (!is.null(rule)) {
    cli::rule(rule, ...)
  }
  
  if (!is.null(line)) {
    cli::cat_line()
  }
}
