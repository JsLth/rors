#' Return ORS conditions
#' @description Return the error and warning messages that ORS returned in the
#' last \code{\link{ors_pairwise}} function call. Also works for
#' \code{\link{ors_shortest_distances}}.
#' @param last \code{[integer]}
#'
#' Number of error lists that should be returned. \code{last = 2L},
#' for example, returns errors from the last two function calls.
#'
#' @export
last_ors_conditions <- function(last = 1L) {
  conditions <- get("cond", envir = ors_cache)

  if (length(conditions)) {
    assert_that(is.numeric(last), last >= 1)
    last <- min(last, length(conditions))
    conditions <- conditions[seq_len(last)]
    class(conditions) <- "ors_condition"
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
                                  call,
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
      message <- fill_empty_error_message(code)
    }

    cond <- paste0(
      ifelse(!is.null(code), "Error code ", ""), code, ": ", msg
    )
    attr(cond, "error") <- TRUE
    store_condition(cond, ts = timestamp, call = call)

    if (abort_on_error) {
      cli::cli_abort(
        c("ORS encountered the following exception:", error),
        call = NULL
      )
    }
  } else {
    warnings <- get_ors_warnings(res)
    message <- warnings$message
    code <- warnings$code

    if (length(code) && length(message)) {
      cond <- vapply(
        seq_along(code),
        FUN.VALUE = character(1),
        function(w) paste0("Warning code ", code[w], ": ", message[w])
      )
      attr(cond, "error") <- FALSE
      store_condition(cond, ts = timestamp, call = call)

      if (warn_on_warning) {
        w_vec <- cli::cli_vec(
          cond,
          style = list(vec_sep = "\f", vec_last = "\f")
        )
        cli::cli_warn(c("ORS returned {length(w_vec)} warning{?s}:", w_vec))
      }
    }
  }
  NULL
}


store_condition <- function(msg, ts, call) {
  if (is.null(attr(msg, "error"))) msg <- NA

  conds <- ors_cache$cond
  last_cond <- conds[[1]]

  if (identical(last_cond$ts, ts)) {
    last_cond$msg <- c(last_cond$msg, msg)
    conds[[1]] <- last_cond
  } else {
    new_cond <- list(msg = msg, ts = ts, call = call)
    conds <- c(list(new_cond), conds)
  }

  assign("cond", conds, envir = ors_cache)
}


handle_missing_directions <- function(.data) {
  route_missing <- is.na(.data)
  conds <- get("cond", envir = ors_cache)[[1]]
  warn_indices <- which(grepl("Warning", conds))
  if (all(route_missing)) {
    cli::cli_warn(c(
      "No routes could be calculated. Is the service correctly configured?",
      cond_tip()
    ))
  } else if (any(route_missing)) {
    cond_indices <- cli::cli_vec(
      which(grepl("Error", conds)),
      style = list(vec_sep = ", ", vec_last = ", ")
    )
    cli::cli_warn(c(
      paste(
        "{length(cond_indices)} route{?s} could not be",
        "calculated and {?was/were} skipped: {cond_indices}"
      ),
      cond_tip()
    ))
  } else if (length(warn_indices)) {
    warn_indices <- cli::cli_vec(
      warn_indices,
      style = list(vec_sep = ", ", vec_last = ", ")
    )
    cli::cli_warn(c(
      paste(
        "ORS returned a warning for {length(warn_indices)}",
        "route{?s}: {warn_indices}"
      ),
      cond_tip()
    ))
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
    "For a list of conditions, call {.code %s}.", callstr
  ))
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
