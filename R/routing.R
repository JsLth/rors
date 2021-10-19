# Title     : Routing functions for datasets with OpenRouteService
# Objective : Calculate routing distances between a source dataset and a
#             destination dataset using the directions service from
#             OpenRouteService
# Created by: Jonas Lieth
# Created on: 17.04.2021


#' Routing between two dataframes
#' @description
#' \code{get_route_lengths} calculates the routing distance between two
#' datasets using the Directions service from ORS.
#'
#' @param source Source dataset that represents points that should be routed
#' from. It can be passed as any two-dimensional base data structure, as a list
#' or as an \code{sf}/\code{sfc} object containing point geometries. Each row
#' or element represents a \code{lon/lat} coordinate pair. The coordinate
#' reference system for the source data is expected to be \code{EPSG:4326}. If
#' an object with \code{length > 2} is passed (not \code{sf}/\code{sfc}), it
#' will be tried to heuristically determine the columns containing coordinates.
#' @param destination Destination dataset that represents point coordinates
#' that are to be routed to. The destination dataset follows the same format
#' requirements as the source dataset.
#'
#' For \code{get_shortest_routes}, this can argument can also be a list of
#' accordingly formatted datasets (as returned by \code{get_nearest_pois}).
#' If a list is passed, each list element corresponds to one row in the source
#' dataset. If a two-dimensional data structure is passed, each row in the
#' source dataset feeds from the entire dataframe.
#' @param profile Character vector. Means of transport as supported by
#' OpenRouteService. For \code{get_route_lengths}, only length-1 vectors are
#' allowed, \code{get_shortest_routes} supports multiple profiles. For a list
#' of active profiles, call \code{\link{get_profiles}}. For details on all
#' profiles, refer to the
#' \href{https://giscience.github.io/openrouteservice/documentation/Tag-Filtering.html}{documentation}.
#' @param units Distance unit for distance calculations (\code{"m"},
#' \code{"km"} or \code{"mi"})
#' @param geometry If \code{TRUE}, returns a \code{sf} object containing route
#' geometries. If \code{FALSE}, returns route distance measures.
#' @param ... Additional arguments passed to the ORS API. This includes all
#' options that modify the routing results. For details on each argument,
#' refer to the
#' \href{https://openrouteservice.org/dev/#/api-docs/v2/directions/{profile}/post}{API playground}
#' and
#' \href{https://github.com/GIScience/openrouteservice-docs#routing-options}{documentation}
#' \describe{
#'  \item{\strong{geometry_simplify}}{Logical length-1 vector specifying
#'                                    whether geometry should be simplified.}
#'  \item{\strong{continue_straight}}{Logical length-1 vector. If \code{FALSE},
#'                                    avoids u-turns and forces the route to
#'                                    keep going straight.}
#'  \item{\strong{avoid_borders}}{Length-1 character vector specifying whether
#'                                to avoid, all borders, only controlled ones
#'                                or none. Only available for \code{driving-*}.}
#'  \item{\strong{avoid_countries}}{Numeric vector listing countries to avoid.
#'                                  Each country is assigned a numeric value.
#'                                  Refer to the ORS documentation. Only
#'                                  available for \code{driving-*}.}
#'  \item{\strong{avoid_features}}{Character vector containing traffic features
#'                                 to avoid (e.g. highways or tunnels).}
#'  \item{\strong{avoid_polygons}}{\code{sf} or \code{sfc} object describing
#'                                 areas to avoid.}
#'  \item{\strong{profile_params}}{Nested list containing restrictions and
#'                                 weightings for \code{driving-hgv},
#'                                 \code{cycling-*}, \code{walking},
#'                                 \code{hiking} and \code{wheelchair}.}
#'  \item{\strong{vehicle_type}}{Length-1 character vector specifying the type
#'                               of heavy goods vehicle. Needed to set
#'                               restrictions for \code{driving-hgv}.}
#'  \item{\strong{preference}}{Length-1 character vector describing the routing
#'                             preference. Either "recommended", "fastest" or
#'                             "shortest".}
#'  \item{\strong{radiuses}}{Maximum distance (in m) that road segments can be
#'                           snapped to. \code{radiuses = -1} represents an
#'                           unlimited radius. This option can also be adjusted
#'                           in the ORS service configurations.}
#'  \item{\strong{maximum_speed}}{Numeric length-1 vector specifying the
#'                                maximum speed.}
#' }
#' @returns Dataframe with distances and travel durations between source and
#' destination. If \code{geometry = TRUE}, returns an \code{sf} object
#' containing the route geometries.
#'
#' @section Error handling:
#' Since \code{get_route_lengths} is supposed to conduct a lot of calculations
#' in one go, errors might occur even in well-conceived service setups. In
#' order to make debugging less painful, errors do not tear down the whole
#' process. They are saved to an environment and issue a warning containing the
#' indices of the routes in question. After the process has finished, they can
#' be accessed by calling \code{\link{last_ors_conditions}}. Specific routes
#' can be examined by inspecting its route attributes using
#' \code{\link{get_route_attributes}}.
#'
#' @export
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
                              profile = get_profiles(),
                              units = c("m", "km", "mi"),
                              geometry = FALSE,
                              ...) {
  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE)

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
  
  profile <- match.arg(profile)
  units <- match.arg(units)

  url <- get_ors_url()

  options <- format_ors_options(list(...), profile)

  call_index <- format(Sys.time(), format = "%H:%M:%S")

  extract_lengths <- function(source, dest) {
    route <- query_ors_directions(source = source,
                                  destination = dest,
                                  profile = profile,
                                  units = units,
                                  geometry = geometry,
                                  options = options,
                                  url = url)
    i <- get("i", envir = parent.frame())

    if (!is.null(route$error)) {
      pkg_cache$routing_conditions[[call_index]][i] <- route$error

      if (!geometry) {
        return(data.frame(distance = NA,
                          duration = NA))
      } else {
        return(data.frame(distance = NA,
                          duration = NA,
                          geometry = NA))
      }
    }

    if (!is.null(route$routes$warnings)) {
      pkg_cache$routing_conditions[[call_index]][i] <- route$routes$warnings
    }

    if (!is.null(route$features$warnings)) {
      pkg_cache$routing_conditions[[call_index]][i] <- route$features$warnings
    }

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
    do.call(rbind, .)

  route_missing <- sapply(unlist(route_list), is.na)
  if (all(route_missing)) {
    cli::cli_warn("No routes could be calculated. Check your service config.")
  } else if (any(route_missing)) {
    conds <- pkg_cache$routing_conditions[[length(pkg_cache$routing_conditions)]]
    cond_indices <- cli::cli_vec(which(grepl("Error", conds)),
                                 style = list(vec_sep = ", ", vec_last = ", "))
    cli::cli_warn(c(paste("{length(cond_indices)} route{?s} could not be",
                          "calculated and {?was/were} skipped: {cond_indices}"),
                    "For a list of error messages, call {.fn last_ors_conditions}"))
  }

  if (is.null(route_list$geometry)) {
    return(route_list)
  } else {
    return(sf::st_as_sf(route_list))
  }
}


#' Calculate shortest routes to nearby points of interest
#' @description
#' \code{get_shortest_routes} is a wrapper around \code{get_route_lengths} that
#' matches each point of the source dataset to a list of points of interest
#' from the destination dataset and then extracts the route with the shortest
#' distance.
#'
#' @param proximity_type Type of proximity that the calculations should be
#' based on. If `distance`, the shortest physical distance will be calculated
#' and if `duration`, the shortest temporal distance will be calculated.
#' @returns Dataframe with distances, travel durations and the index number of
#' the point of interest with the shortest distance to the respective place of
#' the source dataset.
#'
#' @export
#'
#' @describeIn get_route_lengths
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
                                destination,
                                profile = get_profiles(),
                                proximity_type = 'duration',
                                units = c("m", "km", "mi"),
                                geometry = FALSE,
                                ...) {
  cli_abortifnot(is.character(profile))
  cli_abortifnot(is.character(proximity_type))

  source <- format_input_data(source)

  if (inherits(destination, "list")) {
    destination <- lapply(destination, format_input_data)
  } else {
    destination <- format_input_data(destination)
  }

  calculate.shortest.routes <- function (profile, point_number) {
    routes <- get_route_lengths(
      source = source[point_number, ],
      destination = if (is.data.frame(destination)) {
        destination
      } else if (is.list(destination)) {
        destination[[point_number]]
      },
      profile = profile,
      units = units,
      geometry = geometry,
      ...
    )

    if (identical(tolower(proximity_type), "distance")) {
      best_index <- suppressWarnings(
        match(min(routes[["distance"]], na.rm = TRUE),
              routes[["distance"]])
      )

    } else if (identical(tolower(proximity_type), 'duration')) {
      best_index <- suppressWarnings(
        match(min(routes[["duration"]], na.rm = TRUE),
              routes[["duration"]])
      )

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
    profile = profile,
    point_number = seq_len(nrow(source))
  ) %>%
    expand.grid()

  cli::cli_progress_bar(name = "Calculating shortest routes...",
                        total = nrow(source) * length(profile),
                        type = "iterator")

  # Find shortest route for each coordinate pair
  route_list <- purrr::pmap(nested_iterator,
                             ~calculate.shortest.routes(.x, .y)) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    cbind(nested_iterator[["point_number"]], nested_iterator[["profile"]], .)

  cli::cli_progress_done()

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


#' Get route attributes
#' @description Calls the directions service of ORS and returns route segments
#' along with a set of additional attributes
#' @param source Any kind of numeric vector containing \code{x/y} values of a
#' route segment that should be routed from. Refer to
#' \code{\link{get_route_lengths}}.
#' @param destination Any kind of numeric vector containing \code{x/y} values
#' of a route segment that should be routed to.
#' @param profile Character vector. Means of transport as supported by
#' OpenRouteService.
#' @param features List of additional information to be included in the
#' output. A feature is included for each route segment, i.e. for each
#' linestring in the output. Possible values include "avgspeed", "detourfactor",
#' "percentage", "elevation", "steepness", "suitability", "surface",
#' "waycategory", "waytype", "tollways", "traildifficulty", "osmid",
#' "roadaccessrestrictions", "countryinfo", "green" and "noise". If
#' \code{TRUE}, all features are taken into account. Refer to the
#' \href{https://github.com/GIScience/openrouteservice-docs#routing-response}{routing response documentation}
#' @inheritParams get_route_lengths
#' @returns A dataframe containing linestrings and attributes for each route
#' segment
#' @seealso get_route_lengths
#'
#' @export

inspect_route <- function(source,
                          destination,
                          profile = get_profiles(),
                          units = c("m", "km", "mi"),
                          attributes = list(),
                          elevation = TRUE,
                          extra_info = list(),
                          by_waypoint = FALSE, # TODO: Implement
                          elev_as_z = TRUE, # TODO: Implement
                          ...) {
  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE)

  # Bring input data into shape
  source <- format_input_data(source)
  destination <- format_input_data(destination)

  verify_crs(source, crs = 4326)
  verify_crs(destination, crs = 4326)

  profile <- match.arg(profile)
  units <- match.arg(units)

  url <- get_ors_url()

  features <- list(attributes = attributes,
                   elevation = elevation,
                   extra_info = extra_info)
  options <- format_ors_options(append(features, list(...)), profile)

  res <- query_ors_directions(source = source,
                              destination = destination,
                              profile = profile,
                              units = units,
                              geometry = TRUE,
                              options = options,
                              url = url)

  geometry <- ors_multiple_linestrings(res, by_waypoints = FALSE)

  distances <- calculate_distances(geometry)
  durations <- calculate_durations(res, distances$distance)
  speeds <- calculate_avgspeed(distances$distance, durations$duration)

  if (is.element("avgspeed", options$attributes)) {
    avgspeed <- res$features$properties$segments[[1]]$avgspeed
  } else avgspeed <- NULL

  if (is.element("detourfactor", options$attributes)) {
    detourfactor <- res$features$properties$segments[[1]]$detourfactor
  } else avgspeed <- NULL

  ascent <- res$features$properties$segments[[1]]$ascent
  descent <- res$features$properties$segments[[1]]$descent

  extra_info <- vapply(options$extra_info,
                       function(x) format_extra_info(res, x, by_waypoint),
                       data.frame(1))

  elements <- list(
    distances,
    durations,
    speeds,
    percentages,
    extra_info,
    geometry
  )

  elements <- elements[lengths(elements) != 0]
  route <- do.call(data.frame, elements)

  route_sf <- structure(
    sf::st_as_sf(route),
    avgspeed = if (exists("avgspeed")) avgspeed,
    detourfactor = if (exists("detourfactor")) detourfactor,
    ascent = ascent,
    descent = descent
  )
  route_sf
}