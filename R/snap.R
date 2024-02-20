#' Snap to nearest street
#'
#' @description
#' Queries the snap endpoint to snap source points to the nearest accessible
#' street. This function does not work with the public API.
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
#' @examples
#' \dontrun{
#' set.seed(123)
#'
#' samp <- ors_sample(50)
#' plot(samp, col = "black")
#'
#' snap1 <- ors_snap(samp)
#' plot(snap1, add = TRUE, col = "red")
#'
#' snap2 <- ors_snap(samp, radius = 5000)
#' plot(snap2, add = TRUE, col = "blue")
#'
#' snap2 <- ors_snap(samp, profile = "foot-walking")
#' plot(snap2, add = TRUE, col = "green")
#' }
#' @export
ors_snap <- function(src,
                     profile = get_profiles(),
                     radius = 350,
                     instance = NULL,
                     ...) {
  assert_that(is_sf(src), is.numeric(radius))
  profile <- match.arg(profile)
  instance <- check_instance(instance)
  iid <- get_id(instance = instance)

  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, id = iid)
  url <- get_ors_url(id = iid)

  if (is_ors_api(url)) {
    cli::cli_abort("{.fn ors_snap} is not available on the public API.")
  }

  src <- prepare_input(src)

  res <- call_ors_snap(src, profile, radius, url, ...)
  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

  meta <- res$metadata
  snap <- tidy_snap(res)

  structure(
    snap,
    call = match.call(),
    metadata = meta,
    class = c("ors_snap", class(snap))
  )
}


tidy_snap <- function(res) {
  loc <- res$locations
  coords <- loc$location

  # repair and convert geometry
  na_coords <- which(lengths(coords) == 0)
  coords[na_coords] <- rep(list(c(NA_real_, NA_real_)), length(na_coords))
  geom <- sf::st_as_sfc(lapply(coords, sf::st_point))
  loc$location <- NULL

  if (loadable("units")) {
    units(loc$snapped_distance) <- "m"
  }

  sf::st_sf(as_data_frame(loc), geometry = geom, crs = 4326)
}
