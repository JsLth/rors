# Title     : Get sample data depending on the area covered by the ORS instance
# Objective : Identify the currently running extract, read it and generate
#             sample data
# Created by: Jonas Lieth
# Created on: 29.09.2021


read_extract_boundaries <- function(force_new_extract = FALSE) {
    if (is.null(pkg_cache$extract_boundaries)) {
    extract_path <- identify_extract(force = force_new_extract)
    extract_data <- osmextract::oe_read(
      extract_path,
      layer = "multipolygons",
      query = paste(
        "SELECT geometry FROM \"multipolygons\"",
        "WHERE boundary = \"administrative\"",
        "AND admin_level IS NOT NULL"
      ),
      quiet = TRUE
    ) %>%
      sf::st_geometry() %>%
      sf::st_union()

      assign("extract_boundaries", extract_data, envir = pkg_cache)
      return(extract_data)
    } else {
      return(pkg_cache$extract_boundaries)
    }
}


#' ORS dependent sampling
#' @description Sample point geometries within the boundaries of the running
#' ORS extract.
#'
#' @param size Number of points to be sampled
#' @param ... Passed to \code{\link[sf]{st_sample}, which passes it to
#' \code{\link[base]{sample} or \code{\link[spatstat.core]{rThomas}.
#' @param force_new_extract If TRUE, forces the cached extract path to be
#' overwritten.
#' @returns \code{sfc} object containing the sampled \code{POINT} geometries
#' @details The function reads the extract file as an \code{sf} object.
#' Depending on the file, this can take a while. The unionized boundaries are
#' then cached making subsequent function calls a lot faster.
#'
#' @export

ors_sample <- function(size, ..., force_new_extract = FALSE) {
  extract <- read_extract_boundaries(force_new_extract) %>%
    lonlat_to_utm()
  sample <- sf::st_sample(extract, size, ...) %>%
    st_transform(4326) %>%
    st_geometry()
  return(sample)
}