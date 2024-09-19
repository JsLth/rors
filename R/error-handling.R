#' Return ORS conditions
#' @description List errors and warnings that were produced in the last
#' call to one of the ORS endpoints.
#' @param last \code{[integer]}
#'
#' Number of error lists that should be returned. \code{last = 2L},
#' for example, returns errors from the last two function calls.
#'
#' @examples
#' \dontrun{
#' ors_pairwise(pharma, sf::st_geometry(pharma) + 100)
#' last_ors_conditions()
#' }
#' @export
last_ors_conditions <- function(last = 1L) {
  conditions <- get0("cond", envir = rors_cache)

  if (length(conditions)) {
    assert_that(is.numeric(last), last >= 1)
    last <- min(last, length(conditions))
    conditions <- conditions[seq_len(last)]
    class(conditions) <- "ors_condition_list"
    conditions
  }
}


#' Accepts a result list and handles error and warning codes
#' @param res Response list from `call_ors_directions`
#' @param abort_on_error Whether to abort when an error code is returned
#' @param warn_on_warning Whether to warn when a warning code is returned
#' @noRd
handle_ors_conditions <- function(res,
                                  timestamp,
                                  abort_on_error = FALSE,
                                  warn_on_warning = FALSE) {
  if (is_ors_error(res)) {
    msg <- res$error
    code <- NULL

    if (!is.character(res$error)) {
      msg <- msg$message
      code <- res$error$code
    }

    if (is.null(msg) && !is.null(code)) {
      msg <- fill_empty_error_message(code)
    }

    call <- get_main_caller()
    store_condition(
      code,
      msg,
      index = res$index,
      ts = timestamp,
      call = call,
      error = TRUE
    )

    if (abort_on_error) {
      code <- ifelse(!is.null(code), sprintf(" code %s", code), "")
      msg <- c(
        "!" = "ORS encountered the following exception:",
        sprintf("Error%s: %s", code, msg)
      )
      abort(msg, call = NULL, class = "api_error")
    }
  } else {
    warnings <- get_ors_warnings(res)
    msg <- warnings$message
    code <- warnings$code

    if (length(code) && length(message)) {
      store_condition(
        code,
        msg,
        index = res$index,
        ts = timestamp,
        call = call,
        error = FALSE
      )

      if (warn_on_warning) {
        cond <- paste0("Warning code ", code, ": ", msg)
        style <- list(vec_sep = "\f", vec_last = "\f")
        w_vec <- cli::cli_vec(cond, style = style)
        cli::cli_warn(
          c("ORS returned {length(w_vec)} warning{?s}:", w_vec),
          class = "ors_api_warn"
        )
      }
    }
  }
  NULL
}


store_condition <- function(code, msg, index, ts, call, error) {
  conds <- rors_cache$cond
  last_cond <- conds[[1]]

  if (identical(last_cond$ts, ts)) {
    last_cond$msg <- c(last_cond$msg, msg)
    last_cond$code <- c(last_cond$code, code)
    last_cond$error <- c(last_cond$error, error)
    last_cond$index <- c(last_cond$index, index)
    conds[[1]] <- last_cond
  } else {
    new_cond <- ors_condition(
      code = code,
      msg = msg,
      index = index,
      ts = ts,
      call = call,
      error = error
    )
    conds <- c(list(new_cond), conds)
  }

  assign("cond", conds, envir = rors_cache)
}


ors_condition <- function(code, msg, index, ts, call, error) {
  cond <- list(
    code = code,
    msg = msg,
    index = index,
    ts = ts,
    call = call,
    error = error
  )

  class(cond) <- "ors_condition"
  cond
}


handle_missing_directions <- function(.data) {
  route_missing <- is.na(.data)
  conds <- get0("cond", envir = rors_cache)[[1]]
  if (is.null(conds)) return()
  has_warnings <- any(!conds$error)
  style <- list(vec_sep = ", ", vec_last = ", ")

  # all routes missing
  if (all(route_missing)) {
    cli::cli_warn(c(
      "No routes could be calculated. Is the service correctly configured?",
      cond_tip()
    ), class = "ors_pairwise_fail_warning")

  # some routes missing
  } else if (any(route_missing)) {
    idx <- conds$index[conds$error]
    cond_indices <- cli::cli_vec(idx, style = style)
    cli::cli_warn(c(
      paste(
        "{length(cond_indices)} route{?s} could not be",
        "calculated and {?was/were} skipped: {cond_indices}"
      ),
      cond_tip()
    ), class = "ors_pairwise_skip_warning")
  }

  # routes associated with warnings
  if (has_warnings) {
    idx <- conds$index[!conds$error]
    warn_indices <- cli::cli_vec(idx, style = style)
    cli::cli_warn(c(
      paste(
        "A warning was emitted for {length(warn_indices)}",
        "route{?s}: {warn_indices}"
      ),
      cond_tip()
    ), class = "ors_pairwise_warn_warning")
  }
}


handle_missing_directions_batch <- function(has_cond) {
  if (any(has_cond)) {
    cond_indices <- cli::cli_vec(
      which(has_cond),
      style = list(vec_sep = ", ", vec_last = ", ")
    )

    cli::cli_warn(c(paste(
      "For the following input rows, one or multiple routes",
      "could not be taken into account: {cond_indices}"
    ), cond_tip(sum(has_cond))))
  }
}


cond_tip <- function(last = NULL) {
  callstr <- ifelse(
    is.null(last) || last == 1,
    "rors::last_ors_conditions()",
    sprintf("rors::last_ors_conditions(last = %s)", last)
  )
  callstr <- cli::style_hyperlink(callstr, paste0("rstudio:run:", callstr))
  cli::col_grey(sprintf(
    "Run {.code %s} for a full list of conditions.", callstr
  ))
}


get_main_caller <- function() {
  calls <- sys.calls()
  expr_to_string <- function(x) paste(deparse(x[[1]]), collapse = "\n")
  cand <- vapply(calls, expr_to_string, FUN.VALUE = character(1))
  main_funs <- c(
    "ors_pairwise", "ors_inspect", "ors_matrix",
    "ors_accessibility", "ors_snap", "ors_export"
  )
  cand <- intersect(cand, main_funs)
  last(cand)
}


#' Replaces empty error message strings based on their error code
#' @noRd
fill_empty_error_message <- function(code) {
  switch(
    as.character(code),
    `2000` = "Unable to parse JSON request.",
    `2001` = "Required parameter is missing.",
    `2002` = "Invalid parameter format.",
    `2003` = "Invalid parameter value.",
    `2004` = "Parameter value exceeds the maximum allowed limit.",
    `2006` = "Unable to parse the request to the export handler.",
    `2007` = "Unsupported export format.",
    `2008` = "Empty Element.",
    `2009` = "Route could not be found between locations.",
    `2099` = "Unknown internal error.",
    `6000` = "Unable to parse JSON request.",
    `6001` = "Required parameter is missing.",
    `6002` = "Invalid parameter format.",
    `6003` = "Invalid parameter value.",
    `6004` = "Parameter value exceeds the maximum allowed limit.",
    `6006` = "Unable to parse the request to the export handler.",
    `6007` = "Unsupported export format.",
    `6008` = "Empty Element.",
    `6099` = "Unknown internal error.",
    "Unknown error code"
  )
}
