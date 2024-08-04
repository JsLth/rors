#' ORS-based sampling
#' @description Sample point geometries within the boundaries of the running
#' ORS extract.
#'
#' If the mounted instance is not local, the running ORS extract cannot be
#' identified. In this case, a random bounding box is generated. See
#' \code{\link{get_extract_boundaries}} for details.
#'
#' @param size \code{[integer]}
#'
#' Number of points to be sampled.
#' @param ... Passed to \code{\link[sf]{st_sample}}.
#' @param force_new_extract \code{[logical]}
#'
#' If \code{TRUE}, forces the cached extract geometries to be overwritten.
#' Defaults to \code{FALSE} to increase speed. Set this to \code{TRUE} if the
#' extract has changed and a new one needs to be loaded.
#' @param poly \code{[sf/sfc]}
#'
#' Boundary polygon used to sample points. If \code{NULL}, the default, boundary
#' polygons are extracted from the current instance as set by
#' \code{\link{ors_instance}}.
#' @inheritParams get_extract_boundaries
#' @returns \code{sfc} object containing the sampled \code{POINT} geometries
#' @details The function vectortranslates the extract file to an \code{sf} object.
#' Depending on the file, this can take a while. The unionized boundaries are
#' then cached making subsequent function calls a lot faster.
#'
#' @export
ors_sample <- function(size,
                       ...,
                       force_new_extract = FALSE,
                       instance = NULL,
                       poly = NULL,
                       verbose = TRUE) {
  assert_that(
    assertthat::is.count(size),
    is_true_or_false(force_new_extract),
    is_true_or_false(verbose)
  )

  poly <- poly %||% get_extract_boundaries(instance, force_new_extract, verbose)
  sf::st_sample(poly, size, ...)
}


#' Extract boundaries
#' @description Returns boundary geometries of the currently mounted extract.
#' For local instances, \code{get_extract_boundaries} wraps
#' \code{\link[osmextract]{oe_read}}. For remote instances, see section
#' "Remote instances".
#' @param force \code{[logical]}
#'
#' If \code{TRUE}, extract must be identified and parsed. If
#' \code{FALSE}, the geometries will be read from the cache if possible.
#' @param verbose \code{[logical]}
#'
#' If \code{TRUE}, prints a loading spinner.
#' @param ... If \code{instance} is a local instance, this is ignored.
#' Otherwise, specifies further arguments for generating a random bbox
#' including: \code{dist}, the bbox radius in meters, and \code{seed}, a
#' random seed as in \code{\link{set.seed}}.
#' @returns An \code{sfc} object of the currently mounted extract boundaries.
#' @inheritParams ors_pairwise
#'
#' @section Remote instances:
#'
#' If \code{instance} is not local, it is more difficult to derive the
#' extract boundaries. There is thus far no way of accessing an OSM extract file
#' knowing only the server address. We can, however, make use of some
#' heuristics:
#'
#' \code{\link{ors_export}} can export the built graphs from an ORS server
#' if it allows it. However, it does not work on the public API and it
#' requires knowledge about the approximate area of an extract.
#'
#' \code{\link{ors_guess}} can make an approximation of an extract area. It
#' accesses the snap endpoint which also does not work on the public API and
#' needs to be enabled on other servers. \code{ors_guess} can make a lot of
#' requests and might not be feasible in many situations.
#'
#' @examples
#' \dontrun{
#' # For local instances, reads the extract file
#' bounds <- get_extract_boundaries()
#' plot(bounds)
#'
#' # Subsequent calls recover from the cache
#' get_extract_boundaries()
#' }
#'
#'
#' @export
get_extract_boundaries <- function(instance = NULL,
                                   force = FALSE,
                                   verbose = TRUE,
                                   ...) {
  if (is.null(ors_cache$extract_boundaries) || force) {
    instance <- instance %||% get_instance()

    if (is.null(instance$url)) {
      extract_path <- identify_extract(instance$paths$top)
      if (is.null(extract_path)) {
        abort(
          "Cannot identify current extract file.",
          class = "extract_not_found_error"
        )
      }

      ors_cli(progress = list(
        "step",
        msg = "Reading and processing extract file...",
        msg_done = "Extract file successfully read in!",
        msg_failed = "Extract file could either not be read or not converted.",
        spinner = TRUE
      ))

      proc <- callr::r_bg(admin_from_osm, args = list(extract_path))

      while (proc$is_alive()) {
        ors_cli(progress = "update")
      }

      ors_cli(progress = c(
        progress = "done",
        result = ifelse(nzchar(proc$read_error()), "failed", "done")
      ))

      poly <- proc$get_result()
    } else {
      tip <- paste(
        "{.code get_extract_boundaries} is not usable for unkown remote",
        "servers as the extract boundaries cannot easily be determined.",
        "Consider using {.fn ors_guess}."
      )
      abort(
        c("Cannot get extract from a server URL.", "i" = tip),
        class = "remote_sample_error"
      )
    }

    assign("extract_boundaries", poly, envir = ors_cache)
    poly
  } else {
    ors_cache$extract_boundaries
  }
}


admin_from_osm <- function(path) {
  sql <- paste(
    "SELECT geometry FROM \"multipolygons\"",
    "WHERE boundary = \"administrative\"",
    "AND admin_level IS NOT NULL"
  )
  admin <- suppressWarnings(
    osmextract::oe_read(
      path,
      layer = "multipolygons",
      query = sql,
      quiet = TRUE
    )
  )
  # handle weird OSM files
  admin <- admin[sf::st_is_valid(admin), ]

  # extract only outer boundaries
  sf::st_union(sf::st_geometry(admin))
}
