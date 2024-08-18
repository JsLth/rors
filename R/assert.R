assert_that <- function(..., env = parent.frame(), msg = NULL, add = NULL) {
  tryCatch(
    expr = assertthat::assert_that(..., env = env, msg = msg),
    error = function(e) cli::cli_abort(
      c(e$message, "i" = add),
      call = e$call,
      class = "ors_assert_error"
    )
  )
}

is_true_or_false <- function(x, flag = TRUE) {
  if (flag) {
    isTRUE(x) || isFALSE(x)
  } else {
    is.logical(x) & !is.na(x)
  }
}

is_integerish <- function(x, null = FALSE, string = TRUE) {
  if (string && !null) x <- suppressWarnings(as.numeric(x)) %NA% return(FALSE)
  is.numeric(x) && all(as.integer(x) == x) ||
    ifelse(null, is.null(x), FALSE)
}

is_number <- function(x, null = FALSE) {
  is.numeric(x) && !is.na(x) && length(x) == 1 ||
    ifelse(null, is.null(x), FALSE)
}

is_string <- function(x, null = FALSE) {
  is.character(x) && !is.na(x) && length(x) == 1 ||
    ifelse(null, is.null(x), FALSE)
}

is_sf <- function(x, sfc = TRUE) {
  inherits(x, c("sf", if (sfc) "sfc"))
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
  gtypes <- sf::st_geometry_type(x)
  uni_gtypes <- unique(gtypes)

  if (exclusive) {
    is_type <- all(types %in% uni_gtypes)
  } else {
    is_type <- any(types %in% uni_gtypes)
  }

  if (strict) {
    is_type <- is_type && all(gtypes %in% types)
  }

  is_type
}

get_failure_type <- function(call, env) {
  var <- deparse(call$x)
  if (is.symbol(call$x)) {
    typeof(env[[var]])
  } else {
    typeof(call$x)
  }
}

assertthat::on_failure(is_sf) <- function(call, env) {
  x <- sprintf("{.var %s}", deparse(call$x))
  if (isTRUE(call$sfc)) {
    sprintf("%s is not an {.cls sf} or {.cls sfc} object.", x)
  } else {
    sprintf("%s is not an {.cls sf} dataframe.", x)
  }
}

assertthat::on_failure(is_true_or_false) <- function(call, env) {
  type <- get_failure_type(call, env)
  x <- sprintf("{.var %s}", deparse(call$x))
  if (is.logical(eval(call$x, env))) {
    sprintf("%s is NA, expected {.var TRUE} or {.var FALSE}.", x)
  } else {
    sprintf("%s is of type %s, expected TRUE or FALSE.", x, type)
  }
}

assertthat::on_failure(is_integerish) <- function(call, env) {
  type <- get_failure_type(call, env)
  x <- sprintf("{.var %s}", call$x)
  sprintf("%s is of type %s, expected an integer-like.", x, type)
}

assertthat::on_failure(is_geometry_type) <- function(call, env) {
  x <- sprintf("{.var %s}", deparse(call$x))
  given <- sf::st_geometry_type(eval(call$x, env))
  given <- paste0(unique(given), "s")
  given <- paste(given, collapse = "/")
  types <- eval(call$type)
  types <- paste0(types, "s")

  if (length(types) > 1) {
    types <- paste(
      paste(utils::head(types, -1), collapse = ", "),
      utils::tail(types, 1),
      sep = "/"
    )
  }

  sprintf("%s must consist of only %s, not %s.", x, types, given)
}

assertthat::on_failure(is_number) <- function(call, env) {
  x <- sprintf("{.var %s}", deparse(call$x))
  sprintf("%s must be a single number.", x)
}

assertthat::on_failure(is_string) <- function(call, env) {
  x <- sprintf("{.var %s}", deparse(call$x))
  sprintf("%s must be a single character string.", x)
}
