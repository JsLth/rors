assert_that <- function(..., env = parent.frame(), msg = NULL, add = NULL) {
  tryCatch(
    expr = assertthat::assert_that(..., env = env, msg = msg),
    error = function(e) cli::cli_abort(c("x" = e$message, add), call = e$call)
  )
}


is_sf <- function(x, sfc = TRUE) {
  inherits(x, c("sf", if (sfc) "sfc"))
}

is_true_or_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' Checks whether an sf, sfc or sfg object is of a particular geometry type.
#' Extends st_is by providing additional strictness parameters.
#' @param x sf, sfc or sfg object
#' @param types geometry types
#' @param exclusive specifies whether all geometry types in `types` must be
#' present in `x` or if `x` must only contain at least one of these types.
#' @param strict specifies whether all geometries must be of type `type` or if
#' only some geometries need to be of this type.
#' @noRd
is_geometry_type <- function(x, types, exclusive = TRUE, strict = TRUE) {
  is_type <- lapply(types, function(gt) sf::st_is(x, gt))
  is_type <- do.call(cbind, is_type)

  if (ncol(is_type) > 1) {
    has_type <- as.logical(colSums(is_type))
  } else {
    has_type <- as.logical(sum(is_type))
  }

  if (nrow(is_type) > 1) {
    is_type <- as.logical(rowSums(is_type))
  } else {
    is_type <- as.logical(sum(is_type))
  }

  if (exclusive) {
    has_type <- all(has_type)
  } else {
    has_type <- any(has_type)
  }

  if (strict) {
    is_type <- all(is_type)
  } else {
    is_type <- any(is_type)
  }

  is_type && has_type
}

assertthat::on_failure(is_sf) <- function(call, env) {
  x <- sprintf("{.var %s}", deparse(call$x))
  if (isTRUE(call$sfc)) {
    paste(x, "is not an {.cls sf} or {.cls sfc} object.")
  } else {
    paste(x, "is not an {.cls sf} dataframe.")
  }
}

assertthat::on_failure(is_true_or_false) <- function(call, env) {
  x <- sprintf("{.var %s}", deparse(call$x))
  if (is.logical(eval(call$x, env))) {
    paste(x, "is NA, expected {.var TRUE} or {.var FALSE}.")
  } else {
    paste0(x, " is of type ", typeof(x), ", expected {.var TRUE} or {.var FALSE}.")
  }
}

assertthat::on_failure(is_geometry_type) <- function(call, env) {
  x <- sprintf("{.var %s}", deparse(call$x))
  given <- sf::st_geometry_type(eval(call$x, env))
  given <- paste0(unique(given), "s")
  given <- paste(given, collapse = "/")
  types <- eval(call$type)
  types <- paste0(types, "s")
  types <- paste(
    paste(utils::head(types, -1), collapse = ", "),
    utils::tail(types, 1),
    sep = " or "
  )
  paste0(x, " must consist of only ", types, ", not ", given)
}
