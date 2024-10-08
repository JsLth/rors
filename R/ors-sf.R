#' st_centroid, but returns the input sf instead of an sfc object
#' @noRd
st_centroid2 <- function(polygons) {
  centroids <- sf::st_centroid(polygons)
  sf::st_geometry(polygons) <- centroids
  polygons
}


#' Extracts the smallest linestring increment from ORS directions response
#' @noRd
ors_multiple_linestrings <- function(res, alt = 1L) {
  coordinates <- get_ors_geometry(res, alt, as_coords = TRUE)

  # construct linestrings by splitting and applying sf::st_linestring
  iterator <- seq_len(nrow(coordinates) - 1L)
  linestrings <- lapply(iterator, function(wp) {
    rows <- seq(wp, wp + 1L)
    segment <- coordinates[rows, ]
    sf::st_linestring(segment)
  })
  geometry <- sf::st_sfc(linestrings, crs = 4326L)

  geometry
}


#' Extracts ordered polygon from ORS isochrones response
#' @noRd
ors_polygon <- function(res) {
  coords <- get_ors_geometry(res, alt = NA, as_coords = TRUE)

  # extract properties
  props <- res$features$properties
  props$center <- NULL

  # convert coordinates to polygon geometry
  poly <- coords_to_polygon(coords)
  poly <- sf::st_sf(props, geometry = poly)

  # reverse order of isochrones within each group for proper plotting
  poly <- tapply(
    seq_len(nrow(poly)),
    INDEX = as.factor(poly$group_index),
    FUN = function(i) poly[i, ][rev(seq_len(dim(poly[i, ])[1])), ],
    simplify = FALSE
  )
  poly <- do.call(rbind.data.frame, poly)
  row.names(poly) <- NULL
  poly
}


ors_contours <- function(res, df = FALSE) {
  contours <- res$features$properties$contours
  is_intersection <- !vapply(contours, is.null, logical(1))
  contours <- drop_null(contours)
  coords <- get_ors_geometry(res, alt = NA, as_coords = TRUE)
  poly <- coords_to_polygon(coords[is_intersection])

  contours <- lapply(contours, function(x) {
    colnames(x) <- c("index", "range")
    rownames(x) <- c("a", "b")
    x
  })
  contours <- simplify2array(contours)

  sf::st_as_sf(data_frame(
    a_index = contours["a", "index", ],
    b_index = contours["b", "index", ],
    a_range = contours["a", "range", ],
    b_range = contours["b", "range", ],
    geometry = poly
  ))
}


#' Rasterizes the isochrone polygons from ORS isochrones
#' @noRd
rasterize_isochrones <- function(iso, res) {
  grid <- sf::st_make_grid(iso, n = res)
  grid <- stats::aggregate(iso, by = grid, FUN = min, join = sf::st_intersects)
  grid <- terra::vect(grid)
  base <- terra::rast(grid, ncols = res[1], nrows = res[2])
  terra::rasterize(grid, base, field = "value")
}


#' Check if an object has a CRS
#' @noRd
has_crs <- function(x) {
  !is.na(sf::st_crs(x))
}


coords_to_polygon <- function(coords) {
  if (!is.list(coords)) coords <- list(coords)
  poly <- lapply(coords, function(x) sf::st_polygon(list(x[1, , ])))
  do.call(sf::st_sfc, c(poly, crs = 4326))
}


#' dirty hack to convert an sf object to a geojson string
#' uses st_write to write to a tempfile
#' @noRd
sf_to_geojson <- function(x) {
  tempf <- tempfile(fileext = ".geojson")
  on.exit(unlink(tempf))
  sf::st_write(sf::st_geometry(x), tempf, quiet = TRUE)
  x <- jsonlite::read_json(tempf)
  x$name <- NULL
  class(x) <- "ors_geojson"
  x
}
