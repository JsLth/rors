#' Extract boundaries
#' @description Returns boundary geometries of the currently mounted extract
#' either from the local host or from a local cache.
#' @param force \code{[logical]}
#'
#' If \code{TRUE}, function must query local host. If
#' \code{FALSE}, the status will be read from the cache if possible.
#' @param verbose \code{[logical]}
#'
#' If \code{TRUE}, prints a loading spinner.
#' @returns An \code{sfc} object of the currently mounted extract boundaries.
#' @inheritParams ors_distances
#'
#' @export
get_extract_boundaries <- function(instance = NULL, force = FALSE, verbose = TRUE) {
  if (is.null(ors_cache$extract_boundaries) || force) {
    if (is.null(instance)) {
      instance <- get_instance()
    }

    if (is.null(instance$url)) {
      extract_path <- identify_extract(instance)
      if (is.null(extract_path)) {
        cli::cli_abort("Cannot identify current extract file. Pass it explicitly.")
      }
      
      ors_cli(
        progress = "step",
        msg = "Reading and processing extract file...",
        msg_done = "Extract file successfully read in!",
        msg_failed = "Extract file could either not be read or not converted.",
        spinner = TRUE
      )
      
      proc <- callr::r_bg(
        function(extract_path) {
          extract_data <- suppressWarnings(
            osmextract::oe_read(
              extract_path,
              layer = "multipolygons",
              query = paste(
                "SELECT geometry FROM \"multipolygons\"",
                "WHERE boundary = \"administrative\"",
                "AND admin_level IS NOT NULL"
              ),
              quiet = TRUE
            )
          )
          sf::st_union(sf::st_geometry(extract_data))
        },
        args = list(extract_path)
      )
      
      while (proc$is_alive()) {
        ors_cli(progress = "update")
      }
      
      ors_cli(progress = "done")
      
      poly <- proc$get_result()
    } else {
      if (is_ors_api(instance$url)) {
        poly <- sf::st_as_sfc(sf::st_bbox(c(
          xmin = -180,
          ymin = -90,
          xmax = 180,
          ymax = 90
        ), crs = 4326))
      } else {
        cli::cli_abort(c(
          "Cannot get extract from a server URL.",
          "i" = paste(
            "{.code get_extract_boundaries} is not usable for unkown remote",
            "servers as the extract boundaries cannot easily be determined."
          )
        ))
      }
    }

    assign("extract_boundaries", poly, envir = ors_cache)
    poly
  } else {
    ors_cache$extract_boundaries
  }
}


#' ORS dependent sampling
#' @description Sample point geometries within the boundaries of the running
#' ORS extract.
#'
#' @param size \code{[integer]}
#'
#' Number of points to be sampled.
#' @param ... Passed to \code{\link[sf]{st_sample}}.
#' @param force_new_extract \code{[logical]}
#'
#' If \code{TRUE}, forces the cached extract path to be overwritten.
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
  
  if (is.null(poly)) {
    poly <- get_extract_boundaries(instance, force_new_extract, verbose)
  } else {
    assert_that(
      is_sf(poly),
      is_geometry_type(poly, c("POLYGON", "MULTIPOLYGON"), exclusive = FALSE)
    )
  }

  sample <- sf::st_sample(poly, size, ...)
  sample <- sf::st_sf(geometry = sample)

  sf::st_as_sf(tibble::as_tibble(sample))
}
