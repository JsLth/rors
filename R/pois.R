# Title     : Functions to acquire data regarding points of interest
# Objective : Get points of interest in the proximity of a source dataset
# Created by: Jonas Lieth
# Created on: 18.06.2021


#' Extract points of interest from OpenStreetMap
#' @description Makes Overpass requests to extract points of interest within
#' a specified area or list of areas.
#'
#' @param source Either (1) a source dataset as used for
#' \code{\link{get_route_lengths}}, (2) a character string of a place or
#' region, (3) a spatial polygon as an sf or sfc object or (4) any bbox format
#' supported by \code{\link[osmdata]{opq}}. The source argument is passed to
#' osmdata to derive a bounding box for the Overpass query.
#' @param ... Can be used to pass key-value combinations of OSM map features.
#' Map features are passed as arguments: key = value. For a list of features,
#' refer to the
#' \href{https://wiki.openstreetmap.org/wiki/Map_features}[documentation].
#' @param radius Numeric scalar. Specifies the buffer size if a source dataset
#' is passed (see details).
#' @param crs Any object that is recognized by \code{\link[sf]{st_crs}}.
#' Specifies the coordinate notation of the source dataset, if it is provided
#' (see details).
#' @param trim If TRUE and if a spatial polygon is passed as `source`, trim
#' the output data to the provided polygon shape.
#' @param timeout Timeout limit for the Overpass query. See
#' \code{\link[osmdata]{opq}. If an sf object, a named vector, or a source
#' dataset is passed, this value can be estimated. For any other format, the
#' timeout is fixed to 100 seconds and may need adjustment for larger queries.
#' @param as_sf If TRUE, returns an sf dataframe containing point geometries.
#' If FALSE, returns a dataframe containing coordinates.
#' @returns
#' If a source dataset is provided, either as sf or coordinate pairs, the
#' function will generate bounding boxes from buffers generated around each
#' coordinate pair in the dataframe. This results in a list of dataframes with
#' each element corresponding one coordinate pair in the source dataset.
#' If any other format is provided, the function will return a single dataframe
#' containing all points of interest within the provided area.
#' @details The function tries to transform coordinate notations according to
#' the passed CRS. For vectors or matrices, this does not currently work. So
#' if you need to pass a vecorized bbox, make sure to pass geographic
#' coordinates, preferably EPSG:4326.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' pois_source <- get_osm_pois(datensatz.a, amenity = "hospital", radius = 5000, crs = 4326)
#'
#' test_sf <- sf::st_as_sf(datensatz.a, coords = c(1,2), crs = 4326)
#' test_poly <- sf::st_convex_hull(st_union(test_sf))
#' get_osm_pois(test_poly, amenity = "post_box", building = "residential", trim = FALSE)
#'
#' test_bbox <- as.numeric(sf::st_bbox(test_poly))
#' get_osm_pois(test_bbox, amenity = "marketplace", as_sf = TRUE)
#'
#' test_place <- "Cologne"
#' get_osm_pois(test_place, landuse = "allotments", timeout = 200, as_sf = TRUE)

get_osm_pois <- function(
  source,
  ...,
  radius = 5000,
  crs = NA,
  trim = TRUE,
  timeout = NULL,
  as_sf = FALSE
) {
  if (!missing(...)) {
    named_values <- list(...)
    features <- paste(
      shQuote(names(named_values), type = "cmd"),
      shQuote(named_values, type = "cmd"),
      sep = "="
    )
  } else {
    cli::cli_abort("No map features passed.")
  }

  # Convert sf object to matrix
  if (is.sf(source)) {
    if (st_crs(source) != 4326) source <- st_transform(source, 4326)
    source_coords <- st_geometry(source) %>%
      st_coordinates() %>%
      .[, c(1,2)]
    crs <- st_crs(source)
  } else {
    source_coords <- source
  }

  # If multiple points are passed, generate buffers around them
  if (
    (
      is.data.frame(source) &&
      !is.sf(source)
    ) ||
    (
      is.sf(source) &&
      sf::st_is(source, c("POINT", "MULTIPOINT"))
    )
  ) {
    source_coords <- as.data.frame(source_coords)
    verify_crs(source_coords, crs)

    # Generate a buffer for each point in the source dataset
    bbox <- buffer_bbox_from_coordinates(source_coords, radius, crs) %>%
      pmap(c)

    # Estimate the timeout for point data
    timeout <- round(10 + (nrow(source_coords) * radius / 1000))
  } else {
    bbox <- list(source_coords)
    if(
      is.sf(source) &&
      sf::st_is(source, c("POLYGON", "MULTIPOLYGON"))
    ) {
      # Estimate the timeout for polygon data
      area <- as.numeric(sf::st_area(source))
      timeout <- round(sqrt(area / 1000000))
    } else if (!is.null(names(source))) {
      # Estimate the timeout for named vector bboxes
      area <- sf::st_bbox(source) %>%
        sf::st_as_sfc() %>%
        sf::st_sf(crs = crs) %>%
        sf::st_area() %>%
        as.numeric()
      timeout <- round(sqrt(area / 1000000))
    } else {
      # Fix the timeout for strings, matrices or unnamed vectors
      if (is.null(timeout)) {
        timeout <- 100
      }
    }
  }

  if (timeout < 25) timeout <- 25

  # Function that queries points of interest within a bbox
  query.osm <- function(bbox) {
    osm_data <- osmdata::opq(
      bbox = bbox,
      timeout = timeout
    ) %>%
      osmdata::add_osm_features(features = features) %>%
      osmdata::osmdata_sf()

    if (
      is.sf(source) &&
      sf::st_is(source, c("POLYGON", "MULTIPOLYGON")) &&
      trim
    ) {
      osm_data <- trim_osmdata(osm_data, bbox)
    }
    return(osm_data)
  }
  # Apply `query.osm` to each point in the dataset
  response <- lapply(bbox, query.osm)

  # Calculate the centroids of each output multipolygon
  pois <- lapply(response, function(r) {
      centroids_from_polygons(r$osm_polygons, as_sf = as_sf)
    }
  )

  if (length(pois) == 1) pois <- pois[[1]]

  return(pois)
}


#' Find the nearest points of interest from a local dataset
#' @description Returns points of interest in the proximity of the source
#' dataset. Unlike \code{\link{get_osm_pois}}, this function requires a local
#' dataset of points of interest.
#'
#' @param source Source dataset that represents point coordinates that are to
#' be routed from. The source dataset should be passed as a dataframe or plain
#' nested list with each row representing a x/y or lon/lat coordinate pair.
#' @param pois Dataset that represents a list of points of interest to be
#' routed to for each coordinate pair in the source dataset. The POI dataset
#' can either be passed as a dataframe or plain nested list with each row
#' representing a x/y or lon/lat coordinate pair, or, alternatively, as a
#' simple features object. If radius is not `NULL`, the POI dataset will be
#' converted to an sf object if it is not already.
#' @param number_of_points Integer scalar. Number of points to be returned.
#' @param radius Numeric scalar. Specifies the buffer size to select points of
#' interest by distance
#' @param crs Any object that is recognized by \code{\link[sf]{st_crs}}.
#' Specifies the coordinate notation of the source and pois dataset, if it they
#' are provided as simple coordinates.
#' @returns List of dataframes with each dataframe containing all points of
#' interest in a given type of proximity to the respective source point.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @details The proximity can either be defined by the number of points to be selected, by a
#' distance buffer or by both. If both measures are defined, the function will select points
#' of interest within a certain radius first and will then select a given number of points
#' within that radius. The proximity type can be controlled by either passing or not passing
#' a value to `number_of_points` and `radius`.
#' @examples
#' get_nearest_pois(datensatz.a, osm_pois, 3, NULL)
#' [[1]]
#'          X        Y
#' 1 6.924774 50.99160
#' 2 6.975410 51.03098
#' 3 6.960451 50.98735
#'
#' [[2]]
#'          X        Y
#' 1 7.016889 50.91035
#'  2 7.042753 50.89777
#' 3 6.966869 50.91035
#'
#' ...

get_nearest_pois <- function(
  source,
  pois,
  number_of_points = NULL,
  radius = NULL,
  crs = NA) {
  if (is.numeric(number_of_points)) {
    if (is.null(radius)) {
      # A number of points, but no radius is given -> n.nearest.pois
      return(n.nearest.pois(source, pois, number_of_points))
    }

    else if (is.numeric(radius)) {
      # A number of points and a radius is given -> pois.within.radius and then
      # n.nearest.pois
      pois_within_radius <- pois.within.radius(source, pois, radius)
      number_of_points_within_radius <- pois_within_radius %>%
        length() %>%
        seq_len() %>%
        purrr::map(
          ~n.nearest.pois(
            source[..1, ],
            pois_within_radius[[..1]],
            number_of_points)
        ) %>%
        purrr::flatten()
      return(number_of_points_within_radius)
    }

    else {
      cli::cli_abort('Radius must be either numeric or NULL.')
    }
  }

  else if (is.null(number_of_points) && is.numeric(radius)) {
    # No number of points, but a radius is given -> pois.within.radius
    return(pois.within.radius(source, pois, radius, crs))
  }

  else {
    stop('Either a radius, number of points, or both must be defined.')
  }
}


n.nearest.pois <- function(source, poi_coordinates, n) {
  if(is.sf(poi_coordinates)) {
    poi_coordinates <- reformat_vectordata(poi_coordinates)
  }

  if (is.sf(source)) {
    source <- reformat_vectordata(source)
  }

  create.distance.matrix <- function(point_index) {
    # Creates a one-to-many distance matrix between the one source coordinate
    # and all POI coordinates.
    matrix <- fields::rdist(source[point_index, ], poi_coordinates)
    return(matrix)
  }
  select.lowest.distance <- function(
    number_index,
    matrix_index,
    dist_mat,
    sorted_dist_mat
  ) {
    if (number_index <= nrow(poi_coordinates)) {
      # Selects the POI dataset index with the lowest distance to the
      # respective source coordinate
      index <- which(
        dist_mat[[matrix_index]] == sorted_dist_mat[[matrix_index]][number_index]
      )
      # ... and returns the POIs with the selected index
      return(poi_coordinates[index, ])
    } else {
      # If `n = 3`, but there's only 1 hospital in the POI dataset, don't stop,
      # just warn.
      cli::cli_alert_warning(
        sprintf('Point can only be assigned %1.0f POIs.', number_index)
      )
      return()
    }
  }

  # Creates unsorted distance matrices that keep the index order of the POI dataset so that
  # the shortest distance can easily be matched with their true index
  distance_matrices <- purrr::map(
    seq_len(nrow(source)),
    ~create.distance.matrix(..1)
  )
  # Creates sorted distance matrices to easily get the shortest distance by index number
  sorted_distance_matrices <- purrr::map(
    seq_len(nrow(source)), ~create.distance.matrix(..1) %>%
      sort() %>%
      as.matrix()
  )
  # Creates a dataframe to nest the pmap loop so that it loops over n with each
  # iteration of the source dataset
  nested_iterator <- expand.grid(seq_len(n), seq_len(nrow(source)))
  output <- purrr::pmap(nested_iterator,
                 ~select.lowest.distance(
                   ..1,
                   ..2,
                   distance_matrices,
                   sorted_distance_matrices)) %>%
    # Discard of points where n is larger than the poi dataset
    purrr::discard(sapply(., is.null)) %>%
    dplyr::bind_rows() %>%
    cbind(Var2 = nested_iterator[seq_len(nrow(.)), 'Var2']) %>%
    # Split the output by point number
    dplyr::group_by(Var2) %>%
    dplyr::group_split(.keep = FALSE) %>%
    purrr::map(as.data.frame)
  return(output)
}


pois.within.radius <- function(source, pois, radius, crs = NULL) {
  if (!is.sf(pois)) {
    if (!is.na(crs)) {
      # If the POIs are passed as coordinates and the coordinate notation is
      # known, convert coordinates to features.
      pois <- as.data.frame(pois) %>%
      sf::st_as_sf(coords = c(1,2), crs = crs)
    } else {
      cli::cli_abort(
        paste(
          "If data is passed as coordinates, their CRS must be passed as well."
        )
      )
    }
  }

  if (!is.sf(source)) {
    if (!is.na(crs)) {
      points <- sf::st_as_sf(source, coords = c(1, 2), crs = crs) %>%
        sf::st_transform(st_crs(pois))
    } else {
      cli::cli_abort(
        paste(
          "If data is passed as coordinates, their CRS must be passed as well."
        )
      )
    }
  }

  crs <- sf::st_crs(pois) # Save old crs
  pois <- lonlat_to_utm(pois) # ... then reproject

  # Create distance radius as buffer polygon
  buffers <- buffers_from_points(points, radius)
  select.pois.within <- function (source_index) {
    # Selects all POIs within the respective distance buffer
    buffers[source_index, ] %>%
      sf::st_within(pois, ., sparse = FALSE) %>%
      pois$geometry[.] %>%
      sf::st_transform(crs) %>% # Re-reproject
      sf::st_coordinates()
  }
  selected_pois <- purrr::map(seq_len(nrow(source)), select.pois.within)
  return(selected_pois)
}