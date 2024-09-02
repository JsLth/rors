#' Pairwise routing
#' @description
#' \code{ors_pairwise} calculates the pairwise routing distance between two
#' datasets using the Directions service from ORS. In other words, routes
#' are computed between the \emph{i}th row of \code{src} and \code{dst},
#' respectively.
#'
#' \code{ors_shortest_distances} is a wrapper around \code{ors_pairwise} that
#' matches each point of the source dataset to a destination dataset
#' and then extracts the route with the shortest distance.
#'
#' @param src \code{[sf/sfc]}
#'
#' Source dataset containing point geometries that should be routed from.
#' @param dst \code{[sf/sfc]}
#'
#' Destination dataset containing point geometries that should be routed to.
#' For \code{ors_shortest_distances}, the destination argument can also
#' be a dataframe containing a grouping column specified by the \code{group}
#' argument that indicates which destinations refer to which row in the source
#' dataset (as returned by \code{\link{group_by_proximity}}). This is
#' recommended for large datasets because passing a plain sf dataframe routes
#' from each source point to each point in the entire destination dataset.
#' @param profile \code{[character]}
#'
#' Means of transport as supported by OpenRouteService.
#' Defaults to the first profile in a call to \code{\link{get_profiles}}.
#' For \code{ors_shortest_distances}, \code{profile} can be a character
#' vector, for all other functions it needs to be a character scalar.
#' For details on all profiles, refer to the
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
#' @param progress \code{[logical]}
#'
#' Whether to show a progress bar for longer operations.
#' @param instance \code{[ors_instance]}
#'
#' Object of an OpenRouteService instance that should be used for route
#' computations. It is recommended to use \code{\link{ors_instance}}
#' to set an instance globally. This argument should only be used if activating
#' an instance globally is not feasible.
#' @param ... Additional arguments passed to the ORS API. Convenience way to
#' directly pass arguments of \code{\link{ors_params}}.
#' @param params List of additional arguments passed to the ORS API. See
#' \code{\link{ors_params}} for details.
#'
#' @returns \code{ors_pairwise} returns a dataframe with distances and
#' travel durations between source and destination. Distances are specified
#' in the unit given by the \code{units} arguments and durations are specified
#' in seconds.
#'
#' \code{ors_shortest_distances} returns a dataframe containing distances,
#' travel durations and the index number of the point of interest with the
#' shortest routing distance to the respective place of the source dataset.
#'
#' Depending on the \code{geometry} argument, the output of both functions can
#' either be simple dataframes or objects of class \code{sf} containing the
#' linestring geometries of the respective routes.
#'
#' @details
#' For \code{ors_pairwise}, the profile argument supports only length-1
#' vectors while \code{ors_shortest_distances} supports multiple profiles.
#' \code{ors_shortest_distances} finds the shortest route for each source
#' point and each profile, respectively.
#'
#' @section Error handling:
#' Since \code{ors_pairwise} is supposed to conduct a lot of calculations
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
#' if (any_mounted() && ors_ready()) {
#'   data("pharma")
#'
#'   set.seed(123)
#'   dest <- ors_sample(10)
#'
#'   car <- "driving-car"
#'   bike <- "cycling-regular"
#'
#'   # Running with sf objects
#'   ors_pairwise(pharma, dest, profile = car)
#'
#'   # Running with coordinate pairs
#'   ors_pairwise(pharma, dest, profile = bike)
#'
#'   # Returns route geometries
#'   ors_pairwise(
#'     pharma,
#'     dest,
#'     profile = car,
#'     geometry = TRUE
#'   )
#'
#'   # Returns routes in kilometers
#'   ors_pairwise(
#'     pharma,
#'     dest,
#'     profile = bike,
#'     units = "km"
#'   )
#'
#'   # Running with additional arguments
#'   ors_pairwise(
#'     pharma,
#'     dest,
#'     profile = car,
#'     continue_straight = TRUE,
#'     preference = "fastest"
#'   )
#'
#'   # Finding shortest routes from each point in sample_a to sample_b
#'   ors_shortest_distances(pharma, dest, units = "km")
#'
#'   # Pre-filter the nearest 5 destination points by Euclidian distance
#'   pois <- get_closest_pois(pharma, dest, n = 5)
#'
#'   # Only route from each pharmacy to one of the closest 5 destination points
#'   # respectively. For larger datasets, this can increase performance.
#'   ors_shortest_distances(
#'     pharma,
#'     pois,
#'     group = ".group",
#'     geometry = TRUE
#'   )
#' }
ors_pairwise <- function(src,
                         dst,
                         profile = get_profiles(),
                         units = c("m", "km", "mi"),
                         geometry = FALSE,
                         progress = FALSE,
                         instance = NULL,
                         ...,
                         params = NULL) {
  # Validate arguments
  assert_that(is_sf(src), is_sf(dst), is_true_or_false(geometry))
  profile <- match.arg(profile)
  units <- match.arg(units)
  instance <- check_instance(instance)
  url <- get_ors_url(instance)
  assert_endpoint_available(url, "routing")

  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, url = url)

  # Bring input data into shape
  src <- prepare_input(src, len = NROW(dst))
  dst <- prepare_input(dst, len = NROW(src))
  params <- prepare_ors_params(params %||% list(...), profile, nrow(src))
  params$instructions <- FALSE # keep response slim

  ors_pairwise_raw(
    src = src,
    dst = dst,
    profile = profile,
    units = units,
    geometry = geometry,
    progress = progress,
    instance = instance,
    url = url,
    params = params
  )
}


#' Raw pairwise routing
#' @description
#' Similar to \code{ors_pairwise} but does not make any effort to pre-process
#' its arguments.
#'
#' @param src,dst Dataframe or matrix of WGS84 coordinates
#' @param profile Profile to be used for routing
#' @param units Distance unit
#' @param geometry Whether to return geometries
#' @param instance ORS instance
#' @param params List of additional parameters to the Directions endpoint
#' @noRd
ors_pairwise_raw <- function(src,
                             dst,
                             profile,
                             units,
                             geometry,
                             progress,
                             instance,
                             url,
                             params) {
  ts <- timestamp()
  iter <- seq_len(nrow(src))
  if (progress) {
    iter <- cli::cli_progress_along(iter)
  }

  # Apply a directions query to each row
  res <- lapply(
    iter,
    ors_pairwise_single,
    src = src,
    dst = dst,
    profile = profile,
    units = units,
    geometry = geometry,
    params = params,
    url = url,
    instance = instance,
    timestamp = ts
  )
  res <- do.call(rbind, res)

  handle_missing_directions(res)

  if (is_sf(res)) {
    res <- sf::st_as_sf(data_frame(res))
  } else {
    res <- as_data_frame(res)
  }

  attr(res, "locations") <- list(src = src, dst = dst)
  class(res) <- c("ors_dist", class(res))
  res
}

#' Gets and extracts distance and durations values from a Directions request.
#' @noRd
ors_pairwise_single <- function(index,
                                src,
                                dst,
                                profile,
                                units,
                                geometry,
                                params,
                                url,
                                instance,
                                timestamp) {
  res <- call_ors_directions(
    src = src[index, ],
    dst = dst[index, ],
    profile = profile,
    units = units,
    geometry = geometry,
    params = params,
    url = url,
    token = needs_token(instance$token)
  )

  res$index <- index
  cond <- handle_ors_conditions(res, timestamp)
  get_ors_summary(res, geometry = geometry)
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
#'
#' @export
#' @rdname ors_pairwise
ors_shortest_distances <- function(src,
                                   dst,
                                   group = NULL,
                                   profile = get_profiles(),
                                   units = c("m", "km", "mi"),
                                   geometry = FALSE,
                                   instance = NULL,
                                   ...,
                                   proximity_type = c("duration", "distance"),
                                   progress = TRUE) {
  # Validate arguments
  assert_that(is_sf(src), is_sf(dst), is_true_or_false(geometry))
  units <- match.arg(units)
  proximity_type <- match.arg(proximity_type)
  profile <- match.arg(profile, several.ok = TRUE)

  instance <- check_instance(instance)
  url <- get_ors_url(instance)
  assert_endpoint_available(url, "routing")

  src <- prepare_input(src)
  poi <- prepare_input(dst)

  if (!is.null(group)) {
    assert_that(group %in% names(dst))
    poi <- split(poi, f = dst[[group]])
  }

  # Create a nested iterator that iterates through every point number for each
  # profile
  iter <- list(profile = profile, idx = seq_len(nrow(src)))
  iter <- expand.grid(iter, stringsAsFactors = FALSE)

  if (progress) {
    env <- environment()
    cli::cli_progress_bar(total = nrow(iter))
  }

  args <- list(
    src = src,
    dst = poi,
    units = units,
    geometry = geometry,
    instance = instance,
    url = url,
    type = proximity_type,
    progress = progress,
    ...
  )
  routes <- .mapply(ors_shortest_routes_single, dots = iter, MoreArgs = args)
  routes <- do.call(rbind, routes)

  if (progress) cli::cli_progress_done()

  routes <- cbind(profile = iter$profile, src = iter$idx, routes)
  routes <- as_data_frame(routes)
  row.names(routes) <- NULL

  if (geometry) routes <- sf::st_as_sf(routes)

  handle_missing_directions_batch(routes$has_error)
  routes$has_error <- NULL

  attr(routes, "src") <- src
  attr(routes, "dst") <- dst
  class(routes) <- c("ors_sdist", class(routes))
  routes
}

# ors_pairwise:
# Unit: seconds
#                         expr      min       lq     mean   median       uq      max neval
# ors_shortest_distances(a, b) 1.877403 1.939651 1.961651 1.957744 1.974432 2.364335    50
#
#
# ors_pairwise_raw:
# Unit: seconds
#                         expr      min       lq     mean   median      uq      max neval
# ors_shortest_distances(a, b) 1.364241 1.393004 1.437389 1.407082 1.43569 1.931573    50
ors_shortest_routes_single <- function(profile,
                                       idx,
                                       src,
                                       dst,
                                       units,
                                       geometry,
                                       instance,
                                       url,
                                       type,
                                       progress,
                                       ...) {
  if (progress) {
    cli::cli_progress_update(.envir = parent.frame(2))
  }

  # if matrix, take the whole thing
  # if its not a matrix, its a list - cut it by index
  if (is.null(dim(dst))) dst <- dst[[idx]]
  src <- do.call(rbind, replicate(nrow(dst), src[idx, ], simplify = FALSE))

  # catch warning that will be formatted later
  suppressWarnings({
    routes <- ors_pairwise_raw(
      src = src,
      dst = dst,
      profile = profile,
      units = units,
      geometry = geometry,
      progress = FALSE,
      instance = instance,
      url = url,
      params = list(...)
    )
  })

  winner <- which.min(routes[[type]]) %empty% NA_real_
  cbind(dest = winner, routes[winner, ], has_error = anyNA(routes))
}
