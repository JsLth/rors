#' Guess extract boundaries
#'
#' @description
#' Makes reasonable guesses about the boundaries of the OpenStreetMap extract
#' mounted to the active ORS instance. \code{ors_guess} samples points over
#' a specified area and tries to snap these points to the nearest defined
#' street. It is a simple wrapper around \code{\link{ors_snap}}.
#'
#' This function can be useful to validate an ORS setup or to find the
#' extract area of a remote (non-official) ORS server.
#'
#' @param poly \code{[various]}
#'
#' An \code{sf/sfc} object or any object that can be parsed by
#' \code{\link[sf]{st_bbox}}. If \code{NULL}, defaults to global
#' boundaries. Needed to narrow down the area of guessing. For best results,
#' \code{poly} should be as small and precise as possible.
#'
#' @param n \code{[integer]}
#'
#' Number of points to sample. More points mean slower but more precise
#' guess work.
#'
#' @param type \code{[character]}
#'
#' Spatial sampling type. For details, see \code{\link[sf]{st_sample}}.
#'
#' @param poly_fun \code{[function]}
#'
#' Function to derive a boundary polygon from successfully snapped points.
#' Defaults to \code{\link[sf]{st_convex_hull}}. Other useful functions are
#' \code{\link[sf]{st_concave_hull}} and \code{\link[sf]{st_polygonize}}.
#'
#' @param progress \code{[logical]}
#'
#' Whether to show a progress bar for longer operations.
#'
#' @param ... Further arguments passed to \code{poly_fun}
#' @inheritParams ors_pairwise
#' @inheritParams ors_snap
#'
#' @returns An \code{sfc} polygon that approximately represents the
#' boundaries of the current ORS extract.
#'
#' @note
#' \code{ors_guess} usually has to make a lot of requests.
#' If you are rate-limited, you are \strong{strongly} advised against the usage
#' of this function.
#'
#' @examples
#' \dontrun{
#' bounds <- get_extract_boundaries()
#' bounds <- sf::st_buffer(bounds, 10000)
#' plot(bounds)
#'
#' bounds2 <- ors_guess(bounds)
#' plot(bounds2, add = TRUE)
#'
#' bounds3 <- ors_guess(bounds, n = 10000)
#' plot(bounds3, add = TRUE)
#'
#' bounds4 <- ors_guess(bounds, poly_fun = sf::st_concave_hull, ratio = 0)
#' plot(bounds4, add = TRUE)}
#' @export
ors_guess <- function(poly = NULL,
                      n = 1000,
                      radius = 1000,
                      type = "regular",
                      poly_fun = sf::st_convex_hull,
                      instance = NULL,
                      ...) {
  if (is.null(poly)) {
    poly <- sf::st_as_sfc(sf::st_bbox(c(
      xmin = -180,
      ymin = -90,
      xmax = 180,
      ymax = 90
    ), crs = 4326))
  }

  if (!is_sf(poly)) {
    poly <- sf::st_as_sfc(sf::st_bbox(poly), crs = 4326)
  }

  samp <- sf::st_sample(poly, size = n, type = type)

  res <- tryCatch(
    expr = ors_snap(samp, radius = radius, instance = instance, ...),
    error = function(e) {
      if (startsWith(e$body %||% "", "Error code 8010")) {
        cli::cli_abort(
          "Cannot guess the extract area based on the bbox.",
          class = "ors_guess_error",
          call = parent.frame(4)
        )
      } else {
        stop(e)
      }
    }
  )

  poly_fun(sf::st_union(res), ...)
}
