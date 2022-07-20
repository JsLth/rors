#' Extract boundaries
#' @description Returns boundary geometries of the currently mounted extract
#' either from the local host or from a local cache.
#' @param force \code{[logical]}
#' 
#' If \code{TRUE}, function must query local host. If
#' \code{FALSE}, the status will be read from the cache if possible.
#' @param id \code{[character]}
#' 
#' ID or name of a container that contains the OSM extract file whose boundaries
#' should be extracted. If \code{NULL}, the default, retrieves the ID from the
#' current instance as set by \code{\link[ORSRouting]{ors_instance}}.
#' @param verbose \code{[logical]}
#' 
#' If \code{TRUE}, prints a loading spinner.
#' @returns An \code{sfc} object of the currently mounted extract boundaries.
#' 
#' @export

get_extract_boundaries <- function(id = NULL, force = FALSE, verbose = TRUE) {
  if (is.null(ors_cache$extract_boundaries) || force) {
    id <- get_id(id)
    if (is_url(id)) {
      cli::cli_abort(c(
        "Cannot get extract from a server URL.",
        "i" = paste(
          "{.code get_extract_boundaries} is only usable for self-built servers,",
          "otherwise the extract file cannot easily be determined."
        )
      ))
    }
    extract_path <- identify_extract(force = force, id = id)
    
    if (interactive() && verbose) {
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
      if (interactive() && verbose) cli::cli_progress_update()
    }

    if (interactive() && verbose) cli::cli_progress_done()
    
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
#' @param size \code{[integer]}
#' 
#' Number of points to be sampled.
#' @param ... Passed to \code{\link[sf]{st_sample}}, which passes it to
#' \code{\link[base]{sample}} or \code{\link[spatstat.core]{rThomas}}.
#' @param force_new_extract \code{[logical]}
#' 
#' If \code{TRUE}, forces the cached extract path to be overwritten.
#' @param poly \code{[sf/sfc]}
#' 
#' Boundary polygon used to sample points. If \code{NULL}, the default, boundary
#' polygons are extracted from the current instance as set by
#' \code{\link[ORSRouting]{ors_instance}}.
#' @inheritParams get_extract_boundaries
#' @returns \code{sfc} object containing the sampled \code{POINT} geometries
#' @details The function vectortranslates the extract file to an \code{sf} object.
#' Depending on the file, this can take a while. The unionized boundaries are
#' then cached making subsequent function calls a lot faster.
#'
#' @export

ors_sample <- function(size, ..., force_new_extract = FALSE, id = NULL, poly = NULL, verbose = TRUE) {
  if (is.null(poly)) {
    poly <- get_extract_boundaries(force_new_extract, id, verbose)
  }
  
  sample <- sf::st_sample(poly, size, ...)

  sf::st_sf(geometry = sample)
}
