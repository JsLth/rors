# Title     : Get sample data depending on the area covered by the ORS instance
# Objective : Identify the currently running extract, read it and generate
#             sample data
# Created by: Jonas Lieth
# Created on: 29.09.2021


#' Extract boundaries
#' @description Returns boundary geometries of the currently mounted extract
#' either from the local host or from a local cache.
#' @param force If \code{TRUE}, function must query local host. If
#' \code{FALSE}, the status will be read from the cache if possible.
#' @returns An \code{sfc} object of the currently mounted extract boundaries.
#' 
#' @export

get_extract_boundaries <- function(force = FALSE) {
  if (is.null(pkg_cache$extract_boundaries) || force) {
    extract_path <- identify_extract(force = force)
    extract_data <- suppressWarnings(
      osmextract::oe_read(extract_path,
                          layer = "multipolygons",
                          query = paste("SELECT geometry FROM \"multipolygons\"",
                                        "WHERE boundary = \"administrative\"",
                                        "AND admin_level IS NOT NULL"),
                          quiet = TRUE)
    ) %>%
      sf::st_geometry() %>%
      sf::st_union()

      assign("extract_boundaries", extract_data, envir = pkg_cache)
      extract_data
    } else {
      pkg_cache$extract_boundaries
    }
}


#' ORS dependent sampling
#' @description Sample point geometries within the boundaries of the running
#' ORS extract.
#'
#' @param size Number of points to be sampled
#' @param ... Passed to \code{\link[sf]{st_sample}}, which passes it to
#' \code{\link[base]{sample}} or \code{\link[spatstat.core]{rThomas}}.
#' @param as_sf If \code{TRUE}, returns an \code{sfc} object, if \code{FALSE},
#' returns a dataframe containing coordinates.
#' @param force_new_extract If \code{TRUE}, forces the cached extract path to
#' be overwritten.
#' @returns \code{sfc} object containing the sampled \code{POINT} geometries
#' @details The function reads the extract file as an \code{sf} object.
#' Depending on the file, this can take a while. The unionized boundaries are
#' then cached making subsequent function calls a lot faster.
#'
#' @export

ors_sample <- function(size, ..., as_sf = FALSE, force_new_extract = FALSE) {
  extract <- get_extract_boundaries(force_new_extract) %>%
    lonlat_to_utm()
  sample <- sf::st_sample(extract, size, ...) %>%
    sf::st_transform(4326) %>%
    sf::st_geometry()
  if (isTRUE(as_sf)) {
    sample
  } else {
    sf::st_coordinates(sample) %>%
      as.data.frame()
  }

}