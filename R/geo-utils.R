#' st_centroid, but returns the input sf instead of an sfc object
#' @noRd
st_centroid2 <- function(polygons) {
  centroids <- sf::st_centroid(polygons)
  sf::st_geometry(polygons) <- centroids
  polygons
}


#' Extracts the smallest linestring increment from ORS directions response
#' @noRd
ors_multiple_linestrings <- function(res, elev_as_z = FALSE) {
  coordinates <- res$features$geometry$coordinates[[1]]
  cols <- seq(1L, 2L + isTRUE(elev_as_z))

  split_ls <- function(wp) {
    rows <- seq(wp, wp + 1L)
    segment <- coordinates[rows, cols]
    sf::st_linestring(segment)
  }

  if (ncol(coordinates) == 3L && isFALSE(elev_as_z)) {
    elevation <- coordinates[, 3L]
    units(elevation) <- "m"
  } else elevation <- NULL

  iterator <- seq_len(nrow(coordinates) - 1L)
  linestrings <- lapply(iterator, split_ls)
  last_point <- sf::st_point(coordinates[nrow(coordinates), cols])
  geometry <- append(linestrings, list(last_point))
  structure(sf::st_sfc(geometry, crs = 4326L), elevation = elevation)
}


#' Extracts ordered polygon from ORS isochrones response
#' @noRd
ors_polygon <- function(res) {
  poly <- lapply(res$features$geometry$coordinates, function(c) {
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


#' st_coordinates but returns a data.frame instead of a matrix
#' @noRd
st_coordinates2 <- function(x) {
  tibble::as_tibble(sf::st_coordinates(x))[, c("X", "Y")]
}
