#' Routing distance computations
#' @description
#' \code{ors_distances} calculates the routing distance between two
#' datasets using the Directions service from ORS. \code{ors_shortest_distances}
#' is a wrapper around \code{ors_distances} that matches each point of the
#' source dataset to a dataset of points of interest from the destination dataset
#' and then extracts the route with the shortest distance.
#'
#' @param source \code{[sf]}
#'
#' Source dataset containing point geometries that shall be routed from.
#' @param destination \code{[sf]}
#'
#' Destination dataset containing point geometries that shall be routed from.
#' The destination dataset follows the same format requirements as the source
#' dataset. For \code{ors_shortest_distances}, the destination argument can also
#' be a dataframe containing a grouping column specified by the \code{group}
#' argument that indicates which destinations refer to which row in the source
#' dataset (as returned by \code{\link{get_closest_pois}}). This is
#' recommended for large datasets because passing a plain sf dataframe routes
#' from each source point to each point in the entire destination dataset.
#' @param profile \code{[character]}
#'
#' Character vector. Means of transport as supported by OpenRouteService. For a
#' list of active profiles, call \code{\link{get_profiles}}. For details on all
#' profiles, refer to the
#' \href{https://giscience.github.io/openrouteservice/documentation/Tag-Filtering.html}{documentation}.
#' @param units \code{[character]}
#'
#' Distance unit for distance calculations (\code{"m"}, \code{"km"} or
#' \code{"mi"})
#' @param geometry \code{[logical]}
#'
#' If \code{TRUE}, returns a \code{sf} object containing route geometries. If
#' \code{FALSE}, returns route distance measures. Defaults to \code{FALSE}, to
#' increase performance.
#' @param instance \code{[ors_instance]}
#'
#' Object of an OpenRouteService instance that should be used for route
#' computations. It is recommended to use \code{\link{ors_instance}}
#' to set an instance globally. This argument should only be used if activating
#' an instance globally is not feasible.
#' @param ... Additional arguments passed to the ORS API. This includes all
#' options that modify the routing results. For details on each argument,
#' refer to the
#' \href{https://openrouteservice.org/dev/#/api-docs/v2/directions/{profile}/post}{API playground}
#' and
#' \href{https://giscience.github.io/openrouteservice/documentation/routing-options/Routing-Options.html}{documentation}
#' \describe{
#'  \item{\strong{geometry_simplify}}{Whether geometry should be simplified.
#'                                    Defaults to \code{FALSE}.}
#'  \item{\strong{continue_straight}}{Whether to avoid u-turns and continue
#'                                    straight if possible. If \code{FALSE},
#'                                    allows the route to take u-turns.
#'                                    Defaults to \code{FALSE}.}
#'  \item{\strong{avoid_borders}}{Character vector specifying whether
#'                                to avoid, \code{all} borders, only
#'                                \code{controlled} ones or \code{none}. Only
#'                                available for \code{driving-*}.}
#'  \item{\strong{avoid_countries}}{Numeric vector listing countries to avoid.
#'                                  Each country is assigned a numeric value.
#'                                  Refer to the \href{https://giscience.github.io/openrouteservice/documentation/routing-options/Country-List}{ORS country list}.
#'                                  Only available for \code{driving-*}.}
#'  \item{\strong{avoid_features}}{Character vector containing traffic features
#'                                 to avoid. One or several of \code{highways},
#'                                 \code{tollways}, \code{ferries}, \code{fords},
#'                                 and \code{steps}.}
#'  \item{\strong{avoid_polygons}}{\code{sf} or \code{sfc} object describing
#'                                 areas to avoid.}
#'  \item{\strong{restrictions}}{Restrictions to be applied for routing.
#'                              Routes that do not conform to these restrictions
#'                              are not considered. Can either be specifications
#'                              for heavy-goods vehicles (\code{axleload},
#'                              \code{hazmat}, \code{height}, \code{length},
#'                              \code{weight}, \code{width}), indicators
#'                              of wheelchair accessibility
#'                              (\code{maximum_incline},
#'                              \code{maximum_sloped_kerb}, \code{minimum_width},
#'                              \code{smoothness_type}, \code{surface_type},
#'                              \code{track_type}), or the maximum allowed
#'                              route steepness (\code{gradient}). Not
#'                              applicable for any other profile. Refer to the
#'                              \href{https://giscience.github.io/openrouteservice/documentation/routing-options/Routing-Options.html}{documentation}
#'                              for details on each parameter.}
#'  \item{\strong{weightings}}{Weightings for route selection. Can be 
#'                             preference multiplicators for walking profiles
#'                             (\code{green}, \code{quiet}) or
#'                             \code{steepness_difficulty} for cycling profiles.
#'                             Not applicable for any other profile.
#'                             Refer to the
#'                             \href{https://giscience.github.io/openrouteservice/documentation/routing-options/Routing-Options.html}{documentation}
#'                             for details on each parameter.}
#'  \item{\strong{allow_unsuitable}}{Whether possibly unsuitable surfaces
#'                                   should be included for wheelchair routing.
#'                                   If \code{TRUE}, includes routes, even
#'                                   if their surface quality might not be
#'                                   suitable for wheelchair driving. Defaults
#'                                   to \code{FALSE}.}
#'  \item{\strong{surface_quality_known}}{Whether to enforce that the surface
#'                                        quality of routes is considered.
#'                                        If \code{FALSE}, routes of all surface
#'                                        qualities are taken into account.
#'                                        Defaults to \code{FALSE}}
#'  \item{\strong{vehicle_type}}{The type of heavy-goods vehicle that is to be
#'                               assumed for profile \code{driving-hgv}. One of
#'                               \code{hgv}, \code{agricultural},
#'                               \code{delivery}, \code{forestry}, and
#'                               \code{goods}. Needed to set restrictions for
#'                               \code{driving-hgv}. Defaults to \code{hgv}.}
#'  \item{\strong{preference}}{Specifies the route preference mode. One of
#'                             \code{recommended}, \code{fastest} or
#'                             \code{shortest}. Defaults to \code{recommended}.}
#'  \item{\strong{radiuses}}{Maximum distance (in m) that road segments can be
#'                           snapped to. \code{radiuses = -1} represents an
#'                           unlimited radius. Defaults to the
#'                           \code{maximum_snapping_radius} in the configuration
#'                           file (350m if not changed).}
#'  \item{\strong{maximum_speed}}{Maximum speed that routing vehicles are
#'                                allowed to drive. Not applied by default.}
#' }
#' @returns \code{ors_distances} returns a dataframe with distances and
#' travel durations between source and destination.
#' \code{ors_shortest_distances} returns a dataframe containing distances,
#' travel durations and the index number of the point of interest with the
#' shortest routing distance to the respective place of the source dataset.
#' Depending on the \code{geometry} argument, these outputs can either be
#' simple dataframes or objects of class \code{sf} containing the linestring
#' geometries of the respective routes.
#'
#' @details
#' For \code{ors_distances}, the profile argument supports only length-1
#' vectors while \code{ors_shortest_distances} supports multiple profiles.
#' \code{ors_shortest_distances} finds the shortest route for each source
#' point and each profile, respectively.
#'
#' @section Error handling:
#' Since \code{ors_distances} is supposed to conduct a lot of calculations
#' in one go, errors might occur even in well-conceived service setups. In
#' order to make debugging less painful, errors do not tear down the whole
#' process. They are saved to an environment and issue a warning containing the
#' indices of the routes in question. After the process has finished, they can
#' be accessed by calling \code{\link{last_ors_conditions}}. Specific routes
#' can be examined by inspecting its route attributes using
#' \code{\link{ors_inspect}}.
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
#' car <- "driving-car"
#' bike <- "cycling-regular"
#'
#' # Running with sf objects
#' route_lengths_sf <- ors_distances(source_sf, dest_sf, profile = car)
#' route_lengths_sf
#'
#' # Running with coordinate pairs
#' route_lengths_df <- ors_distances(source_df, dest_df, profile = bike)
#' route_lengths_df
#'
#' # Returns route geometries
#' route_lengths_geom <- ors_distances(
#'   source_df,
#'   dest_df,
#'   profile = car,
#'   geometry = TRUE
#' )
#'
#' # Returns routes in kilometers
#' route_lengths_km <- ors_distances(
#'   source_df,
#'   dest_df,
#'   profile = bike,
#'   units = "km"
#' )
#'
#' # Running with additional arguments
#' route_lengths_opts <- ors_distances(
#'   source_df,
#'   dest_df,
#'   profile = car,
#'   continue_straight = TRUE,
#'   preference = "fastest"
#' )
#'
#' # Finding shortest routes from each point in sample_a to sample_b
#' shortest_routes <- ors_shortest_distances(source_df, dest_df, units = "km")
#' shortest_routes
#'
#' # Finding the shortest routes to the nearest hospitals
#' pois <- get_osm_pois(sf::st_bbox(source_sf), amenity = "hospital")
#'
#' nearest_hospitals <- ors_shortest_distances(
#'   source,
#'   pois,
#'   geometry = TRUE
#' )
#' nearest_hospitals
#' }
ors_distances <- function(source,
                          destination,
                          profile = get_profiles(),
                          units = c("m", "km", "mi"),
                          geometry = FALSE,
                          instance = NULL,
                          ...) {
  assert(geometry, class = "logical", len = rep(1, 2))
  profile <- match.arg(profile)
  units <- match.arg(units)
  instance <- check_instance(instance)
  iid <- get_id(instance = instance)

  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, id = iid)

  # Bring input data into shape
  source <- format_input_data(source, len = nrow(destination))
  destination <- format_input_data(destination, len = nrow(source))

  locations <- df_nest(source = source, dest = destination)
  url <- get_ors_url(id = iid)
  options <- format_ors_options(list(...), profile)
  call_index <- format(Sys.time(), format = "%H:%M:%OS6")

  # Apply a directions query to each row
  route_df <- lapply(
    seq_len(nrow(locations)),
    FUN = apply_directions,
    locations = locations,
    profile = profile,
    units = units,
    geometry = geometry,
    options = options,
    url = url,
    instance = instance,
    call_index = call_index
  )
  route_df <- do.call(rbind, route_df)

  handle_missing_directions(route_df, call_index)

  if (requireNamespace("units")) {
    units(route_df$distance) <- units
    units(route_df$duration) <- "s"
  }

  if (is_sf(route_df)) {
    route_df <- sf::st_as_sf(tibble::as_tibble(route_df))
  } else {
    route_df <- tibble::as_tibble(route_df)
  }

  structure(
    route_df,
    call = match.call(),
    data = locations,
    class = c("ors_dist", class(route_df))
  )
}


#' Calculate shortest routes to nearby points of interest
#' @param group \code{[character/numeric]}
#'
#' Column name or index providing a grouping column that indicates which row
#' in the destination dataset corresponds to which row in the source dataset
#' (as in the output of \code{\link{get_closest_pois}}). Providing
#' a grouping column can considerably reduce the processing load for larger
#' datasets.
#' @param proximity_type \code{[character]}
#'
#' Type of proximity that the calculations should be
#' based on. If `distance`, the shortest physical distance will be calculated
#' and if `duration`, the shortest temporal distance will be calculated.
#' @param progress \code{[logical]}
#' 
#' If \code{TRUE}, displays a progress bar if the process is taking a bit
#' longer.
#'
#' @export
#'
#' @rdname ors_distances
#'
#' @examples
#' \dontrun{
#'
#' }
ors_shortest_distances <- function(source,
                                   destination,
                                   group = NULL,
                                   profile = get_profiles(),
                                   units = c("m", "km", "mi"),
                                   geometry = FALSE,
                                   instance = NULL,
                                   ...,
                                   proximity_type = c("duration", "distance"),
                                   progress = TRUE) {
  instance <- check_instance(instance)
  proximity_type <- match.arg(proximity_type)
  profile <- match.arg(profile, several.ok = TRUE)
  
  source <- format_input_data(source, to_coords = FALSE)
  destination <- format_input_data(destination, to_coords = FALSE)

  if (!is.null(group)) {
    destination <- split(destination, f = destination$.group)
  }

  # Create a nested iterator that iterates through every point number for each
  # profile
  nested_iterator <- expand.grid(
    list(profile = profile, point_number = seq_len(nrow(source))),
    stringsAsFactors = FALSE
  )
  
  # Find shortest route for each coordinate pair
  route_df <- lapply(seq_len(nrow(nested_iterator)), function(i) {
    ors_cli(progress = "update", .envir = parent.frame(3L))
    apply_shortest_routes(
      index = i, source = source, destination = destination,
      iter = nested_iterator, units = units, geometry = geometry,
      instance = instance, type = proximity_type, ...
    )
  })
  
  route_df <- cbind(
    profile = nested_iterator[["profile"]],
    source = nested_iterator[["point_number"]],
    do.call(rbind, route_df)
  )
  route_df <- tibble::as_tibble(route_df)
  
  rownames(route_df) <- NULL

  if (geometry) route_df <- sf::st_as_sf(route_df)

  handle_missing_directions_batch(route_df$has_error)
  route_df$has_error <- NULL

  structure(
    route_df,
    call = match.call(),
    source = source,
    dest = destination,
    class = c("ors_sdist", class(route_df))
  )
}
