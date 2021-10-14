# Title     : Routing functions for datasets with OpenRouteService
# Objective : Calculate routing distances between a source dataset and a
#             destination dataset using the directions service from
#             OpenRouteService
# Created by: Jonas Lieth
# Created on: 17.04.2021


#' Routing between two dataframes
#' @description Calculates the routing distance between two datasets.
#'
#' @param source Source dataset that represents point coordinates that are to
#' be routed from. The source dataset should be passed as a double nested
#' dataframe with each row representing a x/y or lon/lat coordinate
#' pair or as an \code{sf} or \code{sfc} object containing point geometries.
#' @param destination Destination dataset that represents point coordinates
#' that are to be routed to. The destination dataset follows the same format
#' requirements as the source dataset.
#' @param profile Character scalar. Means of transport as supported by
#' OpenRouteService. For a list of active profiles, call
#' \code{\link{get_profiles}}. For details on all profiles, refer to the
#' \href{https://giscience.github.io/openrouteservice/documentation/Tag-Filtering.html}{documentation}.
#' @param units Distance unit for distance calculations ('m', 'km' or 'mi')
#' @param geometry If \code{TRUE}, returns a \code{sf} object containing route
#' geometries. If \code{FALSE}, returns route distance measures.
#' @param ... Additional arguments passed to the ORS API. This includes all
#' options that modify the routing results. For details on each argument,
#' refer to the
#' \href{https://openrouteservice.org/dev/#/api-docs/v2/directions/{profile}/post}{API playground}
#' and
#' \href{https://github.com/GIScience/openrouteservice-docs#routing-options}{documentation}
#' \itemize{
#'  \item{geometry_simplify}{Logical length-1 vector specifying whether
#'                           geometry should be simplified.}
#'  \item{continue_straight}{Logical length-1 vector. If \code{FALSE}, avoids
#'                           u-turns and forces the route to keep going
#'                           straight}
#'  \item{avoid_borders}{Length-1 character vector specifying whether to avoid
#'                       all borders, only controlled ones or none.}
#'  \item{avoid_countries}{Numeric vector listing countries to avoid. Each
#'                         country is assigned a numeric value. Refer to the
#'                         ORS documentation.}
#'  \item{avoid_features}{Traffic features to avoid (e.g. highways or tunnels)}
#'  \item{avoid_polygons}{\code{sf} or \code{sfc} object describing areas to
#'                        avoid.}
#'  \item{preference}{Length-1 character value describing the routing
#'                    preference. Either "recommended", "fastest" or
#'                    "shortest"}
#'  \item{radiuses}{Maximum distance (in m) that road segments can be snapped
#'                  to. \code{-1} represents an unlimited radius. This option
#'                  can also be adjusted in the ORS service configurations.}
#'  \item,{maximum_speed}{Numeric length-1 vector specifying the maximum speed.}
#' }
#' @returns Dataframe with distances and travel durations between source and
#' destination. If \code{geometry = TRUE}, returns an \code{sf} object
#' containing the route geometries.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' set.seed(111)
#' source <- ors_sample(10)
#' source_sf <- ors_sample[1:5, ]
#' source_df <- as.data.frame(sf::st_coordinates(ors_sample[6:10, ]))
#'
#' set.seed(222)
#' dest <- ors_sample(10)
#' dest_sf <- ors_sample[1:5, ]
#' dest_df <- as.data.frame(sf::st_coordinates(ors_sample[6:10, ]))
#'
#' profile <- get_profiles()[1]
#'
#' # Running with sf objects
#' route_lengths_sf <- get_route_lengths(source_sf, dest_sf, profile)
#' route_lengths_sf
#'
#' # Running with coordinate pairs
#' route_lengths_df <- get_route_lengths(source_df, dest_df, profile)
#' route_lengths_df
#'
#' # Returns route geometries
#' route_lengths_geom <- get_route_lengths(source_df, dest_df, profile, geometry = TRUE)
#'
#' # Returns routes in kilometers
#' route_lengths_km <- get_route_lengths(source_df, dest_df, profile, units = "km")
#'
#' # Running with additional arguments
#' route_lengths_opts <- get_route_lengths(source_dd, dest_df, profile, continue_straight = TRUE, preference = "fastest")

get_route_lengths <- function(source,
                              destination,
                              profile,
                              units = "m",
                              geometry = FALSE,
                              ...) {
  # Check if ORS is ready to use
  if (!ors_ready(force = FALSE)) {
    cli::cli_abort("ORS service is not reachable.")
  }

  # Bring input data into shape
  source <- format_input_data(source)
  destination <- format_input_data(destination)

  verify_crs(source, crs = 4326)
  verify_crs(destination, crs = 4326)

  # If directions is the method of choice but the input suggests one-to-many,
  # replicate the one-element dataframe `nrow` times
  if (nrow(source) == 1) {
    source <- dplyr::bind_rows(replicate(nrow(destination),
                                         source,
                                         simplify = FALSE))

  } else if (nrow(destination) == 1) {
    destination <- dplyr::bind_rows(replicate(nrow(destination),
                                              source,
                                              simplify = FALSE))
  }

  if (identical(row(source), row(destination))) {
    # If both datasets have the same shape, prepare a rowwise iterator for pmap.
    zipped_locations <- data.frame(source = source, dest = destination) %>%
      dplyr::group_split(dplyr::row_number(), .keep = FALSE) %>%
      purrr::map_df(tidyr::nest,
                    source = dplyr::starts_with("source"),
                    dest = dplyr::starts_with("dest"))

  } else {
    source_shape <- cli::cli_vec(nrow(source), style = list(vec_last = ":"))
    dest_shape <- cli::cli_vec(nrow(destination), style = list(vec_last = ":"))

    cli::cli_abort(c(paste("Datasets have non-matching number of rows.",
                           "Can only handle one-to-many, many-to-many,",
                           "or many-to-one calls."),
                     "Source dataset rows: {source_shape}",
                     "Destination dataset rows: {dest_shape}"))
  }

  options_url <- getOption("ors_url")
  url <- ifelse(is.null(options_url),
                sprintf("http://localhost:%s/", get_ors_port()),
                options_url)

  if (!is.null(c(...))) {
    options <- format_ors_options(list(...), profile)
  }

  extract_lengths <- function(source, dest) {
    route <- query_ors_directions(source = source,
                                  destination = dest,
                                  profile = profile,
                                  units = units,
                                  geometry = geometry,
                                  options = options,
                                  url = url)

    if (!geometry) {

      return(data.frame(distance = route$routes$summary$distance,
                        duration = route$routes$summary$duration))

    } else {

      return(data.frame(distance = route$features$properties$summary$distance,
                        duration = route$features$properties$summary$duration,
                        geometry = route$features$geometry))

    }
  }

  # Apply a directions query to each row
  route_list <- zipped_locations %>%
    purrr::pmap(extract_lengths) %>%
    dplyr::bind_rows()

  if (is.null(route_list$geometry)) {
    return(route_list)
  } else {
    return(sf::st_as_sf(route_list))
  }
}



#' Calculate shortest routes to nearby points of interest
#' @description Calculates the shortest routes from a source dataset to the
#' points of interest of each coordinate pair. This function is a wrapper
#' around `get_route_lengths` that matches each coordinate pair to a list of
#' points of interest and returns the route with the shortest distance.
#'
#' @param source Source dataset that represents point coordinates that are to
#' be routed from. The source dataset should be passed as a double nested
#' dataframe or list with each row representing a x/y or lon/lat coordinate
#' pair.
#' @param pois Dataset containing points of interest that should be
#' routed to. The POI dataset can either be passed as a single dataframe or as
#' a list of dataframes. If a list is passed, each list element corresponds to
#' one row in the source dataset. If a dataframe, an \code{sf} object or an
#' \code{sfc} object is passed, each row in the source dataset feeds from the
#' entire dataframe.
#' @param profiles Character vector or list. Means of transport as supported by
#' OpenRouteService. For details, see \code{\link{get_route_lengths}}. This
#' function supports
#' @param proximity_type Type of proximity that the calculations should be
#' based on. If `distance`, the shortest physical distance will be calculated
#' and if `duration`, the shortest temporal distance will be calculated.
#' @param ... Passed to \code{\link{get_route_lengths}}
#' @returns Dataframe with distances, travel durations and the index number of
#' the point of interest with the shortest distance to the respective place of
#' the source dataset.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' source <- ors_sample(5)
#' pois <- get_osm_pois(sf::st_bbox(source), amenity = "hospital")
#'
#' shortest_routes <- get_shortest_routes(source, pois, profiles = c("driving-car", "foot-walking"))
#' shortest_routes
#'    point_number   route_type poi_number distance duration
#' 1             1  driving-car          4  23479.2   1394.6
#' 2             1 foot-walking         53  13806.2   9940.4
#' 3             2  driving-car         59   6783.0    649.9
#' 4             2 foot-walking         60   6047.9   4354.4
#' 5             3  driving-car         34   8751.6    788.4
#' 6             3 foot-walking         35   9093.1   6547.0
#' 7             4  driving-car         19   3009.7    405.3
#' 8             4 foot-walking         20   2320.2   1670.5
#' 9             5  driving-car         13  16882.8   1294.3
#' 10            5 foot-walking         53  13636.7   9818.3

get_shortest_routes <- function(source,
                                pois,
                                profiles = get_profiles(),
                                proximity_type = 'duration',
                                ...) {
  cli_abortifnot(is.character(profiles))
  cli_abortifnot(is.character(proximity_type))

  source <- format_input_data(source)

  if (inherits(pois, "list")) {
    pois <- lapply(pois, format_input_data)
  } else {
    pois <- format_input_data(pois)
  }

  calculate.shortest.routes <- function (profile, point_number) {
    routes <- get_route_lengths(
      source = source[point_number, ],
      destination = if (is.data.frame(pois)) {
        pois
      } else if (is.list(pois)) {
        pois[[point_number]]
      },
      profile = profile,
      ...
    )

    if (identical(tolower(proximity_type), "distance")) {
      best_index <- match(min(routes[["distance"]]),
                          routes[["distance"]])

    } else if (identical(tolower(proximity_type), 'duration')) {
      best_index <- match(min(routes[["duration"]]),
                          routes[["duration"]])

    } else {
      cli::cli_abort(paste("Expected a proximity type",
                           "({.val duration} or {.val distance})"))
    }

    best_route <- cbind(best_index, routes[best_index,])

    cli::cli_progress_update(.envir = parent.frame(3))
    return(best_route)
  }

  # Create a nested iterator that iterates through every point number for each
  # profile
  nested_iterator <- list(
    profiles = profiles,
    point_number = seq_len(nrow(source))
  ) %>%
    expand.grid()

  cli::cli_progress_bar(name = "Calculating shortest routes...",
                        total = nrow(source) * length(profiles),
                        type = "iterator")

  # Find shortest route for each coordinate pair
  route_list <- purrr::pmap(nested_iterator,
                             ~calculate.shortest.routes(.x, .y)) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    cbind(nested_iterator[["point_number"]], nested_iterator[["profiles"]], .)

  cli::cli_progress_done()

  geometry <- isTRUE(list(...)$geometry)

  output_cols <- c("point_number",
                   "route_type",
                   "poi_number",
                   "distance",
                   "duration",
                   if (geometry) "geometry")

  colnames(route_list) <- output_cols
  rownames(route_list) <- NULL

  if (!geometry) {
    return(route_list)
  } else {
    return(sf::st_as_sf(route_list))
  }
}



create_dist_matrix <- function(source,
                               destination,
                               profile,
                               proximity_type = "distance",
                               units = "m") {
  cli_abortifnot(is.character(proximity_type))
  cli_abortifnot(is.character(units))

  if (all(!is.element(proximity_type, c("distance", "duration")))) {
    cli::cli_abort(paste("Expected a proximity type",
                         "({.val {\"duration\"}} or {.val {\"distance\"}})"))
  }

  source <- format_input_data(source)
  destination <- format_input_data(destination)

  port <- get_ors_port()
  options_url <- getOption("ors_url")
  url <- ifelse(is.null(options_url),
                sprintf("http://localhost:%s/", port),
                options_url)

  route <- query_ors_matrix(source = source,
                            destination = destination,
                            profile = profile,
                            metrics = proximity_type,
                            units = units,
                            url = url)

  if (length(proximity_type) == 1) {
    matrix <- route[[paste0(proximity_type, "s")]]
  } else {
    matrix <- list(distances = route$distances,
                   durations = route$durations)
  }
  return(matrix)
}
