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
  if (is.null(ors_cache$extract_boundaries) || force) {
    extract_path <- identify_extract(force = force)
    
    if (interactive()) {
      cli::cli_progress_step(
        msg = "Reading and processing extract file...",
        msg_done = "Extract file successfully read in!",
        msg_failed = "Extract file could either not be read or not converted.",
        spinner = TRUE
      )
    }
    
    proc <- callr::r_bg(
      function(extract_path) {
        extract_data <- suppressWarnings(
          osmextract::oe_read(
            extract_path,
            layer = "multipolygons",
            query = paste("SELECT geometry FROM \"multipolygons\"",
                          "WHERE boundary = \"administrative\"",
                          "AND admin_level IS NOT NULL"),
            quiet = TRUE
          )
        )
        sf::st_union(sf::st_geometry(extract_data))
      },
      args = list(extract_path)
    )
    
    while (proc$is_alive()) {
      if (interactive()) cli::cli_progress_update()
    }

    if (interactive()) cli::cli_progress_done()
    
    extract_geom <- proc$get_result()
    
    assign("extract_boundaries", extract_geom, envir = ors_cache)
    extract_geom
  } else {
    ors_cache$extract_boundaries
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
  extract <- lonlat_to_utm(get_extract_boundaries(force_new_extract))
  sample <- sf::st_sample(extract, size, ...)
  sample <- sf::st_geometry(sf::st_transform(sample, 4326L))
  if (isTRUE(as_sf)) {
    sample
  } else {
    as.data.frame(sf::st_coordinates(sample))
  }
}
