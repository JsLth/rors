#' st_centroid, but returns the input sf instead of an sfc object
#' @noRd
st_centroid2 <- function(polygons) {
  centroids <- sf::st_centroid(polygons)
  sf::st_geometry(polygons) <- centroids
  polygons
}


#' st_coordinates but returns a data.frame instead of a matrix
#' @noRd
st_coordinates2 <- function(x) {
  tibble::as_tibble(sf::st_coordinates(x))[, c("X", "Y")]
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
  poly <- lapply(coords, function(c) {
    ls <- sf::st_linestring(matrix(c, ncol = 2))
    sf::st_sf(geometry = sf::st_sfc(sf::st_cast(ls, "POLYGON")))
  })

  poly <- do.call(rbind, poly)
  poly <- cbind(poly, res$features$properties[-3])
  poly <- sf::st_set_crs(poly, 4326)

  poly <- tapply(
    seq_len(nrow(poly)),
    INDEX = as.factor(poly$group_index),
    FUN = function(i) {
      poly[i, ][seq(dim(poly[i, ])[1], 1), ]
    },
    simplify = FALSE
  )
  poly <- do.call(rbind.data.frame, poly)
  row.names(poly) <- NULL
  poly
}


#' Rasterizes the isochrone polygons from ORS isochrones
#' @noRd
rasterize_isochrones <- function(isochrones, resolution) {
  if (!requireNamespace("terra")) {
    cli::cli_abort("The {.pkg raster} package is needed to rasterize isochrones.")
  }

  # Transform to projected CRS with global coverage (world mercator)
  isochrones <- sf::st_transform(isochrones, 3395)
  grid <- sf::st_make_grid(isochrones, n = resolution)
  grid <- stats::aggregate(isochrones, by = grid, FUN = min, join = sf::st_intersects)
  grid <- terra::vect(grid)
  rasterized <- terra::rasterize(
    grid,
    terra::rast(grid, resolution = resolution),
    field = "value"
  )
  terra::project(rasterized, y = "epsg:4326", method = "near")
}


#' Check if an object is of class sf or sfc
#' @noRd
is_sf <- function(x) {
  inherits(x, c("sf", "sfc"))
}
