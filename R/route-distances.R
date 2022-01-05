# Title     : Routing distance functions
# Objective : Compute the routing distances between two datasets
# Created by: Jonas Lieth
# Created on: 17.04.2021


#' Routing distance computations
#' @description
#' \code{get_route_lengths} calculates the routing distance between two
#' datasets using the Directions service from ORS.
#' @param source Source dataset that represents points that should be routed
#' from. It can be passed as:
#' \itemize{
#'  \item Any two-dimensional base data structure, e.g. dataframes
#'  \item A nested list
#'  \item An \code{sf}/\code{sfc} object containing point geometries.
#' }
#' Each row represents a \code{lon/lat} coordinate pair. The coordinate
#' reference system for the source data is expected to be \code{EPSG:4326}. If
#' an object with \code{length > 2} is passed (not \code{sf}/\code{sfc}), it
#' will be tried to heuristically determine the columns containing coordinates.
#' @param destination Destination dataset that represents point coordinates
#' that are to be routed to. The destination dataset follows the same format
#' requirements as the source dataset.
#' @param profile Character vector. Means of transport as supported by
#' OpenRouteService. For a list of active profiles, call
#' \code{\link{get_profiles}}. For details on all profiles, refer to the
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
#' @returns \code{get_route_lengths} returns a dataframe with distances and
#' travel durations between source and destination.
#'
#' @details
#' For \code{get_route_lengths}, the profile argument supports only length-1
#' vectors while \code{get_shortest_routes} supports multiple profiles.
#' \code{get_shortest_routes} finds the shortest route for each source
#' point and each profile, respectively.
#'
#' @section Error handling:
#' Since \code{get_route_lengths} is supposed to conduct a lot of calculations
#' in one go, errors might occur even in well-conceived service setups. In
#' order to make debugging less painful, errors do not tear down the whole
#' process. They are saved to an environment and issue a warning containing the
#' indices of the routes in question. After the process has finished, they can
#' be accessed by calling \code{\link{last_ors_conditions}}. Specific routes
#' can be examined by inspecting its route attributes using
#' \code{\link{inspect_route}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(111)
#' source_sf <- ors_sample(10, as_sf = TRUE)
#' source_df <- ors_sample(10)
#'
#' set.seed(222)
#' dest_sf <- ors_sample(10, as_sf = TRUE)
#' dest_df <- ors_sample(10)
#'
#' car = "driving-car"
#' bike = "cycling-regular"
#'
#' # Running with sf objects
#' route_lengths_sf <- get_route_lengths(source_sf, dest_sf, profile = car)
#' route_lengths_sf
#'
#' # Running with coordinate pairs
#' route_lengths_df <- get_route_lengths(source_df, dest_df, profile = bike)
#' route_lengths_df
#'
#' # Returns route geometries
#' route_lengths_geom <- get_route_lengths(source_df,
#'                                         dest_df,
#'                                         profile = car,
#'                                         geometry = TRUE)
#'
#' # Returns routes in kilometers
#' route_lengths_km <- get_route_lengths(source_df,
#'                                       dest_df,
#'                                       profile = bike,
#'                                       units = "km")
#'
#' # Running with additional arguments
#' route_lengths_opts <- get_route_lengths(source_df,
#'                                         dest_df,
#'                                         profile = car,
#'                                         continue_straight = TRUE,
#'                                         preference = "fastest")
#' }

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
    source <- do.call(
      rbind,
      replicate(nrow(destination), source, simplify = FALSE)
    )
  } else if (nrow(destination) == 1) {
    destination <- do.call(
      rbind,
      replicate(nrow(source), destination, simplify = FALSE)
    )
  }

  if (identical(row(source), row(destination))) {
    # If both datasets have the same shape, prepare a rowwise iterator for pmap.
    zipped_locations <- list(source = source, dest = destination)
    attr(zipped_locations, "row.names") <- seq_len(nrow(source))
    class(zipped_locations) <- "data.frame"

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

  extract_lengths <- function(i) {
    res <- query_ors_directions(source = zipped_locations[i, "source"],
                                destination = zipped_locations[i, "dest"],
                                profile = profile,
                                units = units,
                                geometry = geometry,
                                options = options,
                                url = url)
    
    cond <- handle_ors_conditions(res)

    if (isTRUE(attr(cond, "error"))) {
      pkg_cache$routing_conditions[[call_index]][i] <- cond

      if (!geometry) {
        return(data.frame(distance = NA, duration = NA))
      } else {
        empty_line <- sf::st_sfc(sf::st_linestring(), crs = 4326)
        return(sf::st_sf(distance = NA, duration = NA, geometry = empty_line))
      }
    } else if (isFALSE(attr(cond, "error"))) {
      pkg_cache$routing_conditions[[call_index]][i] <- cond
    }

    if (!geometry) {
      data.frame(distance = res$routes$summary$distance,
                 duration = res$routes$summary$duration)
    } else {
      distance <- res$features$properties$summary$distance
      duration <- res$features$properties$summary$duration
      linestring <- sf::st_linestring(res$features$geometry$coordinates[[1]])
      sf::st_sf(distance = distance,
                duration = duration,
                geometry = sf::st_sfc(linestring, crs = 4326))
    }
  }

  # Apply a directions query to each row
  route_list <- do.call(rbind, lapply(seq_len(nrow(zipped_locations)), extract_lengths))

  route_missing <- sapply(unlist(route_list), is.na)
  conds <- pkg_cache$routing_conditions[[call_index]]
  warn_indices <- which(grepl("Warning", conds))
  tip <- cli::col_grey("For a list of conditions, call {.fn last_ors_conditions}.")
  if (all(route_missing)) {
    cli::cli_warn(c("No routes could be calculated. Check your service config.",
                    tip))
  } else if (any(route_missing)) {
    cond_indices <- cli::cli_vec(which(grepl("Error", conds)),
                                 style = list(vec_sep = ", ", vec_last = ", "))
    cli::cli_warn(c(paste("{length(cond_indices)} route{?s} could not be",
                          "calculated and {?was/were} skipped: {cond_indices}"),
                    tip))
  } else if (length(warn_indices)) {
    warn_indices <- cli::cli_vec(warn_indices,
                                 style = list(vec_sep = ", ", vec_last = ", "))
    cli::cli_warn(c(paste("ORS returned a warning for {length(warn_indices)}",
                          "route{?s}: {warn_indices}"),
                    tip))
  }
  
  if (requireNamespace("units")) {
    units(route_list$distance) <- units
    units(route_list$duration) <- "s"
  }

  route_list
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
#' @returns \code{get_shortest_routes} returns a dataframe containing
#' distances, travel durations and the index number of the point of interest
#' with the shortest routing distance to the respective place of the source
#' dataset.
#'
#' Depending on the \code{geometry} argument, these outputs can either be
#' simple dataframes or objects of class \code{sf} containing the linestring
#' geometries of the respective routes.
#'
#' @details
#' For \code{get_shortest_routes}, the destination argument can argument can
#' also be a list of accordingly formatted datasets (as returned by
#' \code{get_nearest_pois}). If a list is passed, each list element corresponds
#' to one row in the source dataset. If a two-dimensional data structure is
#' passed, each row in the source dataset feeds from the entire dataframe.
#'
#' @export
#'
#' @rdname get_route_lengths
#'
#' @examples
#' \dontrun{
#' # Finding shortest routes from each point in sample_a to sample_b
#' shortest_routes <- get_shortest_routes(source_df, dest_df, units = "km")
#' shortest_routes
#'
#' # Finding the shortest routes to the nearest hospitals
#' pois <- get_osm_pois(sf::st_bbox(source_sf), amenity = "hospital")
#'
#' nearest_hospitals <- get_shortest_routes(source,
#'                                          pois,
#'                                          geometry = TRUE)
#' nearest_hospitals
#' }

get_shortest_routes <- function(source,
                                destination,
                                profile = get_profiles(),
                                units = c("m", "km", "mi"),
                                geometry = FALSE,
                                ...,
                                proximity_type = c("duration", "distance")) {
  proximity_type <- match.arg(proximity_type)

  source <- format_input_data(source)

  if (inherits(destination, "list")) {
    destination <- lapply(destination, format_input_data)
  } else {
    destination <- format_input_data(destination)
  }

  calculate_shortest_routes <- function(i) {
    routes <- get_route_lengths(
      source = source[nested_iterator[i, "point_number"], ],
      destination = if (is.data.frame(destination)) {
        destination
      } else if (is.list(destination)) {
        destination[[nested_iterator[i, "point_number"]]]
      },
      profile = nested_iterator[i, "profile"],
      units = units,
      geometry = geometry,
      ...
    )

    if (identical(tolower(proximity_type), "distance")) {
      best_index <- suppressWarnings(
        match(min(routes[["distance"]], na.rm = TRUE),
              routes[["distance"]])
      )

    } else if (identical(tolower(proximity_type), "duration")) {
      best_index <- suppressWarnings(
        match(min(routes[["duration"]], na.rm = TRUE),
              routes[["duration"]])
      )

    } else {
      cli::cli_abort(paste("Expected a proximity type",
                           "({.val duration} or {.val distance}),",
                           "got {.val {proximity_type}}"))
    }

    best_route <- cbind(best_index, routes[best_index, ])
    
    cli::cli_progress_update(.envir = parent.frame(2))
    best_route
  }

  # Create a nested iterator that iterates through every point number for each
  # profile
  nested_iterator <- expand.grid(list(profile = profile,
                                      point_number = seq_len(nrow(source))),
                                 stringsAsFactors = FALSE)

  cli::cli_progress_bar(name = "Calculating shortest routes...",
                        total = nrow(source) * length(profile),
                        type = "iterator")

  # Find shortest route for each coordinate pair
  route_list <- lapply(seq_len(nrow(nested_iterator)), calculate_shortest_routes)
  route_list <- cbind(
    nested_iterator[["point_number"]],
    nested_iterator[["profile"]],
    do.call(rbind, route_list)
  )

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
    route_list
  } else {
    sf::st_as_sf(route_list)
  }
}


#' Routing distance matrix
#' @description Calls the matrix service and returns a routing distance matrix.
#' @inheritParams get_route_lengths
#' @returns If \code{length(proximity_type) == 1}, returns a
#' \code{nrow(source) * nrow(destination)} routing distance matrix. Otherwise,
#' returns a list containing two matrices accordingly.

create_dist_matrix <- function(source,
                               destination,
                               profile = get_profiles(),
                               units = c("m", "km", "mi"),
                               proximity_type = c("distance", "duration")) {
  profile <- match.arg(profile)
  proximity_type <- match.arg(proximity_type)
  units <- match.arg(units)

  source <- format_input_data(source)
  destination <- format_input_data(destination)

  port <- get_ors_port()
  options_url <- getOption("ors_url")
  url <- ifelse(is.null(options_url),
                sprintf("http://localhost:%s/", port),
                options_url)

  res <- query_ors_matrix(source = source,
                          destination = destination,
                          profile = profile,
                          metrics = proximity_type,
                          units = units,
                          url = url)

  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

  if (length(proximity_type) == 1) {
    matrix <- res[[paste0(proximity_type, "s")]]
  } else {
    matrix <- list(distances = res$distances,
                   durations = res$durations)
  }

  matrix
}
