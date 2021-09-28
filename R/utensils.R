# Title     : Auxiliary functions for data wrangling and geoprocessing
# Objective : Do the heavy lifting for the main functions
# Created by: Jonas Lieth
# Created on: 11.06.2021

#' @importFrom magrittr %>%

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
    return(centroids)
  } else {
    return(centroids %>%
      sf::st_coordinates() %>%
      as.data.frame())
  }

}

#' @importFrom magrittr %>%

buffers_from_points <- function(
  points,
  radius,
  cap_style = "ROUND",
  join_style = "ROUND") {
  crs <- sf::st_crs(points)
  points %>%
    lonlat_to_utm() %>%
    sf::st_buffer(radius, endCapStyle = cap_style, joinStyle = join_style) %>%
    lonlat_to_utm(crs = crs, reverse = TRUE)
}

#' @importFrom magrittr %>%

buffer_bbox_from_coordinates <- function(coordinates, radius, crs = 4326) {
  # Convert coordinates to metric points
  points <- coordinates %>%
    sf::st_as_sf(coords = c(1, 2), crs = crs) %>%
    lonlat_to_utm()
  # Generate buffers
  buffers <- points %>%
    sf::st_buffer(dist = radius) %>%
    lonlat_to_utm(crs = crs, reverse = TRUE)
  # Extract bboxes
  buffer_bbox <- buffers %>%
    sf::st_geometry() %>%
    purrr::map(st_bbox) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  return(buffer_bbox)
}

#' @importFrom magrittr %>%

reformat_vectordata <- function(data, crs = 4326) {
  # Extract coordinates from geometries
  data %>%
    sf::st_transform(crs = crs) %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    return()
}


swap_xy <- function(data) {
  data <- data[, c(2, 1)]
  return(data)
}

#' @importFrom magrittr %>%

ctransform <- function(coordinates, from_crs, to_crs) {
  # Convert coordinates to sf features
  coordinates_sf <- sf::st_as_sf(coordinates, coords = c(1, 2), crs = from_crs)
  # Transform sf features and extract coordinates
  transf_coordinates <- sf::st_transform(coordinates_sf, to_crs) %>%
    sf::st_coordinates() %>%
    as.data.frame()
  return(transf_coordinates)
}

#' @importFrom magrittr %>%

parse_proj4string <- function(proj4_string) {
  crs_props <- proj4_string %>%
    strsplit(" ") %>%
    purrr::map(~strsplit(..1, "=")) %>%
    purrr::flatten() %>%
    do.call(data.frame, .)
  names(crs_props) <- as.character(unlist(crs_props[1, ]))
  crs_props <- crs_props[-1, ]
  rownames(crs_props) <- NULL
  return(crs_props)
}

#' @importFrom magrittr %>%

lonlat_to_utm <- function(
  coordinates,
  crs = NULL,
  reverse = FALSE,
  zone = NULL) {
  sf_check <- inherits(coordinates, c("sf", "sfc"))
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
      get.zone <- function(longitudes) {
        longitude_median <- median(longitudes)
        floor((longitude_median + 180) / 6) + 1
      }
      zone <- coordinates %>%
        sf::st_coordinates() %>%
        .[, 1] %>%
        as.vector() %>%
        get.zone()
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


#' @importFrom magrittr %>%

verify_crs <- function(data, crs, silent = FALSE) {
  parsed_crs <- sf::st_crs(crs)
  if (is.na(parsed_crs$wkt)) {
    cli::cli_abort("CRS of type {.cls {class(crs)}} is invalid.")
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
    cli::cli_alert_warning(
      paste(
        "Coordinates either fall outside of the EPSG {.val {crs$epsg}}",
        "boundaries or don't match its coordinate notation."
      )
    )
  }
  return(crs_ok)
}


relativePath <- function(absolute_path) {
  relative_path <- gsub(sprintf("%s|%s/", getwd(), getwd()), "", absolute_path)
  if (relative_path == "") {
    relative_path <- "."
  }
  relative_path
}


ensure_permission <- function(command) {
  paste0(
    switch(
      EXPR = Sys.info()["sysname"],
      Linux = "sudo -S ",
      Darwin = "sudo "
    ),
  command
  )
}