# Title     : Auxiliary functions for data wrangling and geoprocessing
# Objective : Convert or process geo information from the main function
# Created by: Jonas Lieth
# Created on: 14.10.2021


centroids_from_polygons <- function(polygons, as_sf = FALSE) {
  crs <- sf::st_crs(polygons)
  # Polygone zu projiziertem CRS transformieren
  polygons_metric <- polygons %>%
    sf::st_geometry() %>%
    lonlat_to_utm()
  # Zentroide der Polygone berechnen
  centroids <- sf::st_centroid(polygons_metric) %>%
    # Zentroide wieder zu altem CRS transformieren
    lonlat_to_utm(crs, reverse = TRUE)
  if (as_sf) {
    sf::st_geometry(polygons) <- centroids
    polygons
  } else {
    centroid_coords <- as.data.frame(sf::st_coordinates(centroids))
    sf::st_geometry(polygons) <- NULL
    cbind(centroid_coords, polygons)
  }

}


buffers_from_points <- function(points,
                                radius,
                                cap_style = "ROUND",
                                join_style = "ROUND") {
  crs <- sf::st_crs(points)
  lonlat_to_utm(points) %>%
    sf::st_buffer(radius, endCapStyle = cap_style, joinStyle = join_style) %>%
    lonlat_to_utm(crs = crs, reverse = TRUE)
}


buffer_bbox_from_coordinates <- function(coordinates, radius, crs = 4326) {
  # Convert coordinates to metric points
  points <- sf::st_as_sf(coordinates, coords = c(1, 2), crs = crs) %>%
    lonlat_to_utm()

  # Generate buffers
  buffers <- sf::st_buffer(points, dist = radius) %>%
    lonlat_to_utm(crs = crs, reverse = TRUE)

  # Extract bboxes
  buffer_bbox <- sf::st_geometry(buffers) %>%
    purrr::map(sf::st_bbox) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  return(buffer_bbox)
}


ors_multiple_linestrings <- function(res, elev_as_z) {
  coordinates <- res$features$geometry$coordinates[[1]]
  cols <- seq(1, 2 + isTRUE(elev_as_z))

  split_ls <- function(wp) {
    rows <- seq(wp, wp + 1)
    segment <- coordinates[rows, cols]
    sf::st_linestring(segment)
  }

  if (ncol(coordinates) == 3 && isFALSE(elev_as_z)) {
    elevation <- coordinates[, 3]
    units(elevation) <- "m"
  } else elevation <- NULL

  iterator <- seq_len(nrow(coordinates) - 1)
  linestrings <- lapply(iterator, split_ls)
  last_point <- sf::st_point(coordinates[nrow(coordinates), cols])
  geometry <- append(linestrings, list(last_point))
  structure(sf::st_sfc(geometry, crs = 4326), elevation = elevation)
}


reformat_vectordata <- function(data) {
  as.data.frame(sf::st_coordinates(data))
}


swap_xy <- function(data) {
  data[, c(2, 1)]
}


ctransform <- function(coordinates, from_crs, to_crs) {
  # Convert coordinates to sf features
  coordinates_sf <- sf::st_as_sf(coordinates, coords = c(1, 2), crs = from_crs)
  # Transform sf features and extract coordinates
  transf_coordinates <- sf::st_transform(coordinates_sf, to_crs) %>%
    sf::st_coordinates() %>%
    as.data.frame()
  return(transf_coordinates)
}


parse_proj4string <- function(proj4_string) {
  crs_props <- proj4_string %>%
    strsplit(" ") %>%
    lapply(strsplit, split = "=") %>%
    unlist(recursive = FALSE) %>%
    do.call(data.frame, .)
  names(crs_props) <- as.character(unlist(crs_props[1, ]))
  crs_props <- crs_props[-1, ]
  rownames(crs_props) <- NULL
  crs_props
}


lonlat_to_utm <- function(
  coordinates,
  crs = NULL,
  reverse = FALSE,
  zone = NULL
) {
  sf_check <- is.sf(coordinates)
  if (!reverse) {
    if (!sf_check) {
      coordinates <- sf::st_as_sf(coordinates, coords = c(1, 2), crs = crs)
      crs <- sf::st_crs(crs)
    } else {
      crs <- sf::st_crs(coordinates)
    }

    if (!crs$IsGeographic) {
      return(coordinates)
    }

    from_crs_props <- crs$proj4string %>%
      parse_proj4string()
    if (is.null(zone)) {
      get_zone <- function(longitudes) {
        longitude_median <- stats::median(longitudes)
        floor((longitude_median + 180) / 6) + 1
      }
      zone <- coordinates %>%
        sf::st_coordinates() %>%
        .[, 1] %>%
        as.vector() %>%
        get_zone()
    }
    to_crs_wkt <- "+proj=utm +zone=%s +datum=%s +units=m" %>%
      sprintf(zone, from_crs_props$`+datum`) %>%
      sf::st_crs()
    if (!sf_check) {
      transf_coordinates <- sf::st_transform(coordinates, to_crs_wkt) %>%
        sf::st_coordinates() %>%
        as.data.frame()
      return(list(coords = transf_coordinates, zone = zone))
    } else {
      transf_coordinates <- sf::st_transform(coordinates, to_crs_wkt)
      return(transf_coordinates)
    }
  } else {
    if (sf_check) {
      transf_coordinates <- sf::st_transform(coordinates, crs)
    } else {
      to_crs_props <- sf::st_crs(crs)$proj4string %>% parse_proj4string()
      from_crs_wkt <- "+proj=utm +zone=%s +datum=%s +units=m" %>%
        sprintf(zone, to_crs_props$`+datum`) %>%
        sf::st_crs()
      transf_coordinates <- ctransform(coordinates, from_crs_wkt, crs)
    }
    return(transf_coordinates)
  }
}


verify_crs <- function(data, crs, silent = FALSE) {
  parsed_crs <- sf::st_crs(crs)

  if (is.na(parsed_crs$wkt)) {
    cli::cli_abort("CRS {.val {crs}} is invalid.")
  }

  wkt <- parsed_crs$wkt %>%
    unlist() %>%
    strsplit("\n")

  crs_bbox <- sapply(wkt, function(x) grep("BBOX", x, value = TRUE)) %>%
    gsub("\\s+|[A-Z]+|\\[|\\]+,", "", ., perl = TRUE) %>%
    strsplit(",") %>%
    unlist() %>%
    as.numeric()

  if (!parsed_crs$IsGeographic) {
    data <- ctransform(data, parsed_crs, 4326)
  }

  x_ok <- sapply(data[, 1], dplyr::between, crs_bbox[2], crs_bbox[4])
  y_ok <- sapply(data[, 2], dplyr::between, crs_bbox[1], crs_bbox[3])
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