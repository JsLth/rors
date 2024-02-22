#' Routing distance matrix
#' @description Calls the matrix service and returns a routing distance matrix.
#' @param dst \code{[sf/sfc]}
#'
#' Destination dataset containing point geometries that should be routed to.
#' If \code{NULL}, only routes between points in \code{src}.
#' @inheritParams ors_pairwise
#' @returns If \code{length(proximity_type) == 1}, returns a
#' \code{nrow(src) * nrow(dst)} routing distance matrix. Otherwise,
#' returns a list containing two matrices accordingly.
#'
#' @export
ors_matrix <- function(src,
                       dst = NULL,
                       profile = get_profiles(),
                       units = c("m", "km", "mi"),
                       proximity_type = c("distance", "duration"),
                       instance = NULL) {
  instance <- instance %||% get_instance()
  iid <- get_id(instance = instance)

  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, id = iid)

  profile <- match.arg(profile)
  proximity_type <- match.arg(proximity_type)
  units <- match.arg(units)

  src <- prepare_input(src, len = nrow(dst))

  if (is.null(dst)) {
    dst <- src
  } else {
    dst <- prepare_input(dst, len = nrow(src))
  }

  url <- get_ors_url(id = iid)

  res <- call_ors_matrix(
    src = src,
    dst = dst,
    profile = profile,
    metrics = proximity_type,
    units = units,
    url = url,
    token = needs_token(instance$token)
  )

  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

  res[[paste0(proximity_type, "s")]]
}
