# Title     : Auxiliary functions for data wrangling and geoprocessing
# Objective : Convert or process geo information from the main function
# Created by: Jonas Lieth
# Created on: 14.10.2021


centroids_from_polygons <- function(polygons, as_sf = FALSE) {
  crs <- sf::st_crs(polygons)
  # Polygone zu projiziertem CRS transformieren
  polygons_metric <- lonlat_to_utm(sf::st_geometry(polygons))
  
  # Zentroide der Polygone berechnen
  centroids <- lonlat_to_utm(sf::st_centroid(polygons_metric), crs, reverse = TRUE)

  if (as_sf) {
    sf::st_geometry(polygons) <- centroids
    polygons
  } else {
    centroid_coords <- as.data.frame(sf::st_coordinates(centroids))
    sf::st_geometry(polygons) <- NULL
    cbind(centroid_coords, polygons)
  }

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
  poly <- sf:::cbind.sf(res$features$properties, poly)
  poly <- sf::st_set_crs(poly, 4326)
  
  poly
}


rasterize_isochrones <- function(isochrones, resolution) {
  if (!requireNamespace("raster")) {
    cli::cli_abort("The {.pkg raster} package is needed to rasterize isochrones.")
  }
  
  isochrones <- lonlat_to_utm(isochrones)
  
  base_raster <- raster::raster(
    crs = sf::st_crs(isochrones)$proj4string,
    ext = raster::extent(sf::st_bbox(isochrones)),
    resolution = resolution
  )
  
  rasterized <- raster::rasterize(
    x = isochrones,
    y = base_raster,
    field = "value",
    fun = min,
    background = NA
  )
  
  rasterized <- raster::projectRaster(rasterized, crs = 4326, method = "ngb")
  
  max_value <- paste(">", max(isochrones$value))
  values <- rev(unique(rasterized[]))
  codes <- seq(1, length(values))
  levels <- sapply(values, function(v) {
    if (is.na(v)) {
      max_value
    } else {
      paste("<=", v)
    }
  })
  
  rasterized <- raster::reclassify(rasterized, matrix(c(values, codes), ncol = 2))
  rasterized <- raster::ratify(rasterized)
  attributes(rasterized)$data@attributes <- list(data.frame(ID = levels))
  
  rasterized
}


reformat_vectordata <- function(data) {
  as.data.frame(sf::st_coordinates(data))
}


swap_xy <- function(data) {
  data[, c(2L, 1L)]
}


ctransform <- function(coordinates, from_crs, to_crs) {
  # Convert coordinates to sf features
  coordinates_sf <- sf::st_as_sf(coordinates, coords = c(1L, 2L), crs = from_crs)
  
  # Transform sf features and extract coordinates
  transf_coordinates <- sf::st_transform(coordinates_sf, to_crs)
  transf_coordinates <- as.data.frame(sf::st_coordinates(transf_coordinates))
  transf_coordinates
}


parse_proj4string <- function(proj4_string) {
  crs_props <- lapply(strsplit(proj4_string, " "), strsplit, split = "=")
  crs_props <- do.call(data.frame, unlist(crs_props, recursive = FALSE))
  
  names(crs_props) <- as.character(unlist(crs_props[1, ]))
  crs_props <- crs_props[-1L, ]
  rownames(crs_props) <- NULL
  crs_props
}


lonlat_to_utm <- function(
  coordinates,
  crs = NA,
  reverse = FALSE,
  zone = NULL
) {
  sf_check <- is.sf(coordinates)
  if (!reverse) {
    if (!sf_check) {
      if (is.na(crs)) {
        cli::cli_abort("A valid CRS must be passed.")
      } 
      coordinates <- sf::st_as_sf(coordinates, coords = c(1L, 2L), crs = crs)
      crs <- sf::st_crs(crs)
    } else {
      crs <- sf::st_crs(coordinates)
    }

    if (!crs$IsGeographic) {
      return(coordinates)
    }

    from_crs_props <- parse_proj4string(crs$proj4string)
    if (is.null(zone)) {
      get_zone <- function(longitudes) {
        longitude_median <- stats::median(longitudes)
        floor((longitude_median + 180L) / 6L) + 1L
      }
      zone <- get_zone(as.vector(sf::st_coordinates(coordinates)[, 1L]))
    }
    to_crs_wkt <- sf::st_crs(
      sprintf(
        "+proj=utm +zone=%s +datum=%s +units=m",
        zone,
        from_crs_props$`+datum`
      )
    )
    if (!sf_check) {
      transf_coordinates <- sf::st_transform(coordinates, to_crs_wkt)
      transf_coordinates <- as.data.frame(sf::st_coordinates(transf_coordinates))
      transf_coordinates <- list(coords = transf_coordinates, zone = zone)
    } else {
      transf_coordinates <- sf::st_transform(coordinates, to_crs_wkt)
    }
    transf_coordinates
  } else {
    if (sf_check) {
      transf_coordinates <- sf::st_transform(coordinates, crs)
    } else {
      to_crs_props <- parse_proj4string(sf::st_crs(crs)$proj4string)
      from_crs_wkt <- sf::st_crs(
        sprintf(
          "+proj=utm +zone=%s +datum=%s +units=m",
          zone, to_crs_props$`+datum`
        )
      )
      transf_coordinates <- ctransform(coordinates, from_crs_wkt, crs)
    }
    transf_coordinates
  }
}


verify_crs <- function(data, crs, silent = FALSE) {
  parsed_crs <- sf::st_crs(crs)

  if (is.na(parsed_crs$wkt)) {
    cli::cli_abort("CRS {.val {crs}} is invalid.")
  }

  wkt <- strsplit(unlist(parsed_crs$wkt), "\n")

  crs_bbox <- sapply(wkt, function(x) grep("BBOX", x, value = TRUE))
  crs_bbox <- gsub("\\s+|[A-Z]+|\\[|\\]+,", "", crs_bbox, perl = TRUE)
  crs_bbox <- as.numeric(unlist(strsplit(crs_bbox, ",")))

  if (!parsed_crs$IsGeographic) {
    data <- ctransform(data, parsed_crs, 4326L)
  }

  x_ok <- sapply(data[, 1L], function(x, left, right) x >= left & x <= right, crs_bbox[2L], crs_bbox[4L])
  y_ok <- sapply(data[, 2L], function(y, left, right) y >= left & y <= right, crs_bbox[1L], crs_bbox[3L])
  crs_ok <- all(c(x_ok, y_ok))

  if (!silent && !crs_ok) {
    cli::cli_alert_warning(paste0("Coordinates either fall outside of the EPSG:",
                                  "{.val {parsed_crs$epsg}} boundaries or ",
                                  "don't match its coordinate notation."))
  }
  crs_ok
}


is.sf <- function(x) {
  inherits(x, c("sf", "sfc"))
}