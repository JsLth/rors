#' Snap to nearest street
#'
#' @description
#' Queries the snap endpoint to snap source points to the nearest accessible
#' street.
#'
#' @param radius \code{[numeric]}
#'
#' Snapping radius. Points are only snapped to a street if the street lies
#' within this distance to the source point.
#'
#' @param ... Additional arguments passed to the snap API.
#' @inheritParams ors_pairwise
#' @returns A dataframe containing the snapped geometries of each point in
#' \code{src} together with the name of the street and the snapping distance.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot(pharma, col = "black")
#'
#' snap1 <- ors_snap(pharma)
#' plot(snap1, add = TRUE, col = "red")
#'
#' snap2 <- ors_snap(pharma, radius = 5000)
#' plot(snap2, add = TRUE, col = "blue")
#'
#' snap3 <- ors_snap(pharma, profile = "foot-walking")
#' plot(snap3, add = TRUE, col = "green")
#' }
ors_snap <- function(src,
                     profile = get_profiles(),
                     radius = 350,
                     instance = NULL,
                     ...) {
  assert_that(is_sf(src), is.numeric(radius))
  profile <- match.arg(profile)
  instance <- check_instance(instance)
  url <- get_ors_url(instance)
  assert_endpoint_available(url, "snap")

  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, url = url)

  src <- prepare_input(src)

  ts <- timestamp()
  res <- call_ors_snap(
    src,
    profile,
    radius,
    url,
    token = needs_token(instance$token),
    ...
  )
  handle_ors_conditions(res, ts, abort_on_error = TRUE, warn_on_warning = TRUE)

  meta <- res$metadata
  snap <- tidy_snap(res)

  attr(snap, "meta") <- meta
  class(snap) <- c("ors_snap", class(snap))
  snap
}


tidy_snap <- function(res) {
  loc <- res$locations
  if (all(is.na(loc))) {
    abort(
      "All output locations are missing. Cannot tidy snap data.",
      class = "snap_na_error"
    )
  }
  coords <- loc$location

  # repair and convert geometry
  na_coords <- which(lengths(coords) == 0)
  coords[na_coords] <- rep(list(c(NA_real_, NA_real_)), length(na_coords))
  geom <- sf::st_as_sfc(lapply(coords, sf::st_point))
  loc$location <- NULL

  sf::st_sf(as_data_frame(loc), geometry = geom, crs = 4326)
}
