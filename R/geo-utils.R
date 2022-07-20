# Title     : Auxiliary functions for data wrangling and geoprocessing
# Objective : Convert or process geo information from the main function
# Created by: Jonas Lieth
# Created on: 14.10.2021


#' 
centroids_from_polygons <- function(polygons) {
  crs <- sf::st_crs(polygons)
  polygons_metric <- sf::st_geometry(polygons)
  centroids <- sf::st_centroid(polygons_metric)
  sf::st_geometry(polygons) <- centroids
  polygons
}


buffers_from_points <- function(points, radius, cap_style = "ROUND", join_style = "ROUND") {
  crs <- sf::st_crs(points)
  points_metric <- lonlat_to_utm(points)
  buffers <- sf::st_buffer(points_metric,
                           radius,
                           endCapStyle = cap_style,
                           joinStyle = join_style)
  lonlat_to_utm(buffers, crs = crs, reverse = TRUE)
}


buffer_bbox_from_coordinates <- function(coordinates, radius, crs = 4326L) {
  # Convert coordinates to metric points
  points <- sf::st_as_sf(coordinates, coords = c(1L, 2L), crs = crs)

  # Generate buffers
  buffers <- lonlat_to_utm(
    sf::st_buffer(lonlat_to_utm(points), dist = radius),
    crs = crs,
    reverse = TRUE
  )

  # Extract bboxes
  buffer_bbox <- lapply(sf::st_geometry(buffers), sf::st_bbox)
  buffer_bbox <- do.call(rbind.data.frame, buffer_bbox)
  buffer_bbox
}


ors_multiple_linestrings <- function(res, elev_as_z) {
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


rasterize_isochrones <- function(isochrones, resolution) {
  if (!requireNamespace("terra")) {
    cli::cli_abort("The {.pkg raster} package is needed to rasterize isochrones.")
  }
  
  # Transform to projected CRS with global coverage (world mercator)
  isochrones <- sf::st_transform(isochrones, 3395)
  grid <- sf::st_make_grid(isochrones, n = resolution)
  grid <- aggregate(isochrones, by = grid, FUN = min, join = sf::st_intersects)
  grid <- terra::vect(grid)
  rasterized <- terra::rasterize(
    grid,
    terra::rast(grid, resolution = resolution),
    field = "value"
  )
  terra::project(rasterized, y = "epsg:4326", method = "near")
}


is.sf <- function(x) {
  inherits(x, c("sf", "sfc"))
}