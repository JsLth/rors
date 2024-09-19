#' Routing distance matrix
#' @description Calls the matrix service and returns a routing distance matrix.
#' @param dst \code{[sf/sfc]}
#'
#' Destination dataset containing point geometries that should be routed to.
#' If \code{NULL}, only routes between points in \code{src}.
#' @inheritParams ors_pairwise
#' @returns A distance matrix.
#'
#' @export
#'
#' @examples
#' if (any_mounted() && ors_ready()) {
#'   # compute distances from each row to each other row
#'   ors_matrix(pharma)
#'
#'   # if two datasets are provided, route from each row in `src` to each row in `dst`
#'   ors_matrix(pharma[1:4, ], pharma[5:8, ])
#'
#'   # distance matrices can be based on time or physical distance
#'   ors_matrix(pharma, proximity_type = "duration")
#'
#'   # units can be adjusted
#'   ors_matrix(pharma, units = "km")
#' }
ors_matrix <- function(src,
                       dst = NULL,
                       profile = get_profiles(force = FALSE),
                       units = c("m", "km", "mi"),
                       proximity_type = c("distance", "duration"),
                       instance = NULL) {
  instance <- instance %||% get_instance()
  url <- get_ors_url(instance)
  assert_endpoint_available(url, "matrix")

  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, url = url)

  profile <- match.arg(profile)
  proximity_type <- match.arg(proximity_type)
  units <- match.arg(units)

  src <- prepare_input(src, len = nrow(dst))

  if (is.null(dst)) {
    dst <- src
  } else {
    dst <- prepare_input(dst, len = nrow(src))
  }

  res <- call_ors_matrix(
    src = src,
    dst = dst,
    profile = profile,
    metrics = proximity_type,
    units = units,
    resolve_locations = FALSE,
    url = url,
    token = needs_token(instance$token)
  )

  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

  res[[paste0(proximity_type, "s")]]
}
