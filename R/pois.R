# Title     : Functions to acquire data regarding points of interest
# Objective : Get points of interest in the proximity of a source dataset
# Created by: Jonas Lieth
# Created on: 18.06.2021


#' Extract points of interest from OpenStreetMap
#' @description Makes Overpass requests to extract points of interest in the proximity of the source
#' dataset.
#'
#' @param source Source dataset that represents point coordinates that are to be routed
#' from. The source dataset should be passed as a dataframe or plain nested list with
#' each row representing a x/y or lon/lat coordinate pair.
#' @param key Character scalar. OpenStreetMap Map Feature key (e.g. amenity, building)
#' @param value Character scalar. OpenStreetMap Map Feature value (e.g. hospital, hotel)
#' @param radius Numeric scalar. Radius in which POIs are to be searched for
#' @param crs Any object that is recognized by \code{\link[sf]{st_crs}}. Coordinate reference system to determine
#' the coordinate notation of the source dataset
#' @returns List of dataframes that contain coordinate pairs of nearby points of interest
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' pois <- query.osm.pois(datensatz.a, key = 'amenity', value = 'hospital', radius = 5000)
#' pois
#' # [[1]]
#' #          X        Y
#' # 1 6.960451 50.98735
#' # 2 6.924774 50.99160
#' # 3 6.975410 51.03098
#' #
#' # [[2]]
#' #          X        Y
#' # 1 6.957034 50.92491
#' # 2 7.042753 50.89777
#' # 3 7.050427 50.89228
#' # 4 6.966869 50.91035
#' #
#' # ...

get_osm_pois <- function(source, key, value, radius = 5000, crs = 4326) {
  # Generate a buffer for each point in the source dataset
  point.bbox <- buffer_bbox_from_coordinates(source, radius, crs)
  # Function that queries points of interest within a buffer
  query.osm <- function(point) {
    osmdata::opq(bbox = point, timeout = 10 + (nrow(source) * radius / 1000)) %>%
      osmdata::add_osm_feature(key = key, value = value) %>%
      osmdata::osmdata_sf()
  }
  # Apply `query.osm` to each point in the dataset
  response <- point.bbox %>% apply(1, query.osm)
  # Calculate the centroids of each output multipolygon
  pois <- response %>%
    lapply(function(response) {
      centroids_from_polygons(response$osm_polygons) %>%
      as.data.frame()
    }
    )
  return(pois)
}


#' Find the nearest points of interest from a local dataset
#' @description Returns points of interest in the proximity of the source dataset. Unlike
#' \code{\link{get_osm_pois}}, this function requires a local dataset of points of interest.
#'
#' @param source Source dataset that represents point coordinates that are to be routed
#' from. The source dataset should be passed as a dataframe or plain nested list with
#' each row representing a x/y or lon/lat coordinate pair.
#' @param pois Dataset that represents a list of points of interest to be routed to
#' for each coordinate pair in the source dataset. The POI dataset can either be passed as a
#' dataframe or plain nested list with each row representing a x/y or lon/lat coordinate pair,
#' or, alternatively, as a simple features object. If radius is not `NULL`, the POI dataset
#' will be converted to an sf object if it is not already.
#' @param number_of_points Integer scalar. Number of points to be returned.
#' @param radius Numeric scalar. Buffer radius that will select points of interest that fall
#' within it.
#' @param pois_crs Any object that is recognized by \code{\link[sf]{st_crs}}. Coordinate reference system
#' to determine the coordinate notation of the `source` and `pois` dataset. It is assumed that
#' both datasets share the same CRS.
#' @returns List of dataframes with each dataframe containing all points of interest
#' in a given type of proximity to the respective source point.
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

get_nearest_pois <- function(source, pois, number_of_points = NULL, radius = NULL, pois_crs = NULL) {
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
        purrr::map(~n.nearest.pois(source[..1, ], pois_within_radius[[..1]], number_of_points)) %>%
        purrr::flatten()
      return(number_of_points_within_radius)
    }

    else {
      stop('Radius must be either numeric or NULL.')
    }
  }

  else if (is.null(number_of_points) && is.numeric(radius)) {
    # No number of points, but a radius is given -> pois.within.radius
    return(pois.within.radius(source, pois, radius, pois_crs))
  }

  else {
    stop('Either a radius, number of points, or both must be defined.')
  }
}


n.nearest.pois <- function(source, poi_coordinates, n) {
  if(is(poi_coordinates, c('sf', 'sfc'))) {
    # No simple features allowed here. Calculations are done with flat numbers.
    poi_coordinates <- reformat_vectordata(poi_coordinates)
  }
  create.distance.matrix <- function(point_index) {
    # Creates a one-to-many distance matrix between the one source coordinate and all
    # POI coordinates.
    matrix <- fields::rdist(source[point_index, ], poi_coordinates)
    return(matrix)
  }
  select.lowest.distance <- function(number_index, matrix_index, dist_mat, sorted_dist_mat) {
    if (number_index <= nrow(poi_coordinates)) {
      # Selects the POI dataset index with the lowest distance to the respective source coordinate
      index <- which(dist_mat[[matrix_index]] == sorted_dist_mat[[matrix_index]][number_index])
      # ... and returns the POIs with the selected index
      return(poi_coordinates[index, ])
    } else {
      # If `n = 3`, but there's only 1 hospital in the POI dataset, don't stop, just warn.
      warning('Point can only be assigned %1.0f POIs.' %>% sprintf(number_index))
      return()
    }
  }
  # Creates unsorted distance matrices that keep the index order of the POI dataset so that
  # the shortest distance can easily be matched with their true index
  distance_matrices <- purrr::map(seq_len(nrow(source)), ~create.distance.matrix(..1))
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


pois.within.radius <- function(source, pois, radius, pois_crs = NULL) {
  if (!is(pois, c('sf', 'sfc'))) {
    if (!is.null(pois_crs)) {
      # If the POIs are passed as coordinates and the coordinate notation is known,
      # convert coordinates to features.
      pois <- as.data.frame(pois) %>%
      sf::st_as_sf(coords = c(1,2), crs = pois_crs)
    } else {
      stop('If POIs are passed as coordinates for geometric operations, their CRS has to be passed as well.')
    }
  }
  crs <- sf::st_crs(pois) # Save old crs
  pois <- sf::st_transform(pois, 25832) # ... then reproject
  points <- sf::st_as_sf(source, coords = c(1, 2), crs = crs) %>% sf::st_transform(st_crs(pois))
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