handle_missing_directions <- function(.data, call_index) {
  route_missing <- is.na(.data)
  conds <- ors_cache$routing_conditions[[call_index]]
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
    is.null(last),
    sprintf("last_ors_conditions(last = %s)", last),
    "last_ors_conditions()"
  )
  cli::col_grey(sprintf("For a list of conditions, call %s.", callstr))
}