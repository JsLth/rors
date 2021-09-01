# Title     : Routing functions for datasets with OpenRouteService
# Objective : Calculate routing distances between a source dataset and a
#             destination dataset using the directions service from
#             OpenRouteService
# Created by: Jonas Lieth
# Created on: 17.04.2021


# Testdatensatz A
datensatz.a <- data.frame(
  lon = c(6.92632,7.00196,7.03162,6.99624,6.91885),
  lat = c(51.02972,50.88385,50.98915,51.00625,50.91935)
)

# Testdatensatz B
datensatz.b <- data.frame(
  lon = c(7.00524, 6.88638, 6.93490, 6.89956, 6.95876),
  lat = c(50.99847, 50.95235, 51.00619, 51.05668, 50.88194)
)


#' Rowwise routing between two dataframes
#' @description Calculates the routing distance between two datasets.
#'
#' @param source Source dataset that represents point coordinates that are to
#' be routed from. The source dataset should be passed as a double nested
#' dataframe or list with each row representing a x/y or lon/lat coordinate
#' pair.
#' @param destination Destination dataset that represents point coordinates
#' that are to be routed to. The source dataset should be passed as a double
#' nested dataframe or list with each row representing a x/y or lon/lat
#' coordinate pair. Essentially both datasets should have the same format.
#' @param profile Character scalar. Means of transport as supported by
#' OpenRouteService. For a list of active profiles, call
#' \code{\link[ORSRouting:ORSConfig]{ORSConfig$active_profiles}}. For details
#' on all profiles, refer to the
#' \href{https://giscience.github.io/openrouteservice/documentation/Tag-Filtering.html}{documentation}.
#' @param units Distance unit for distance calculations ('m', 'km' or 'mi',
#' default: meters)
#' @param local Logical scalar. Specifies whether requests should be sent to
#' the official web server of OpenRouteService or to the local Docker server
#' set up by \code{\link{ORSInstance}}. For the use with larger datasets, it is
#' advised to setup a local service backend. To query the official web server,
#' an API key has to be provided.
#' @param port Integer scalar. Port that the local server is running on.
#' @param api_key Character scalar. API key for the use of the official web
#' server of OpenRouteService. Only necessary, if `local = FALSE`.
#' @param geometry Specifies whether to return distance values or geometry
#' features.
#' @returns Dataframe with distances and travel durations between source and
#' destination
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' route_lengths <- get_route_lengths(datensatz.a, datensatz.b, 'driving-car')
#' route_lengths
#' #   distance duration
#' # 1  12379.8    798.0
#' # 2  17667.3   1703.2
#' # 3  11659.2   1445.1
#' # 4  14933.1   1522.8
#' # 5   7926.0    876.8

get_route_lengths <- function(
  source,
  destination,
  profile,
  units = "m",
  local = TRUE,
  port = 8080,
  api_key = NULL,
  geometry = FALSE
) {
  if (!identical(dim(source), dim(destination))) {
    stop("Both datasets must have the same shape.")
  }
  if (local) {
    url <- paste0("http://localhost:", port, "/ors/v2/directions/")
  } else if (is(api_key, 'character')) {
    url <- "https://api.openrouteservice.org/v2/directions/"
  } else {
    stop("API key must be passed if queries are not local.")
  }

  extract.lengths <- function(source, dest) {
    route <- query.ors.directions(
      source = source,
      destination = dest,
      profile = profile,
      url = url,
      units = units,
      api_key = api_key,
      geometry = geometry)
    return(
      data.frame(
        distance = route$routes[[1]]$summary$distance,
        duration = route$routes[[1]]$summary$duration
      )
    )
  }

  zipped_locations <- data.frame(
    source = source,
    dest = destination
  ) %>%
    dplyr::group_split(
      dplyr::row_number(),
      .keep = FALSE
    ) %>%
    purrr::map_df(
      tidyr::nest,
      source = dplyr::starts_with("source"),
      dest = dplyr::starts_with("dest"))

  # Routen für jedes Koordinatenpaar berechnen
  route.list <- zipped_locations %>%
    purrr::pmap(extract.lengths) %>%
    dplyr::bind_rows()
  return(route.list)
}

# TODO: Implement one-to-many matrix?
query.ors.directions <- function(
  source,
  destination,
  profile,
  url,
  units,
  api_key,
  geometry
) {
  # Get coordinates in shape
  locations <- list(
    c(as.numeric(source[1]), as.numeric(source[2])),
    c(as.numeric(destination[1]), as.numeric(destination[2]))
  )
  # Create http body of the request
  body <- jsonlite::toJSON(
    list(
      coordinates = locations,
      # Remove the clipping distance limit
      radiuses = rep(list(-1), length(locations)),
      units = units,
      geometry = geometry
    ),
    auto_unbox = TRUE
  )
  header <- httr::add_headers(
    Accept = paste(
      "application/json",
      "application/geo+json",
      "application/gpx+xml",
      "img/png; charset=utf-8",
      sep = ", "
    ),
    # Will not be passed if api_key = NULL
    Authorization = api_key,
    `Content-Type` = "application/json; charset=utf-8"
  )

  # Calculate routes for every profile
  routes <- url %>%
    paste0(profile) %>%
    httr::POST(
      body = body,
      encode = paste0(rep('geo', geometry), 'json'),
      header
    ) %>%
    httr::content()
  return(routes)
}


query.ors.matrix <- function(
  source,
  destinations,
  profile,
  url = "http://localhost:8080/ors/v2/matrix/",
  units = "m",
  api_key = NULL
) {
  destinations_list <- dplyr::group_by(destinations, dplyr::row_number()) %>%
    dplyr::group_split(.keep = FALSE) %>%
    as.list() %>%
    map(as.numeric)

  locations <- append(
    destinations_list,
    list(as.numeric(source)),
    after = 0
  )

  body <- jsonlite::toJSON(
    list(
      locations = locations,
      destinations = seq(length(locations) - 1),
      sources = list(0),
      metrics = list("distance", "duration"),
      units = units
    ),
    auto_unbox = TRUE
  )

    header <- httr::add_headers(
    Accept = paste(
      "application/json",
      "application/geo+json",
      "application/gpx+xml",
      "img/png; charset=utf-8",
      sep = ", "
    ),
    # Will not be passed if api_key = NULL
    Authorization = api_key,
    `Content-Type` = "application/json; charset=utf-8"
  )

    routes <- url %>%
    paste0(profile) %>%
    httr::POST(
      body = body,
      encode = 'json',
      header
    ) %>%
    httr::content()
  return(routes)
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
#' @param poi_coords Dataset containing dataframes of points of interest that
#' are to be routed to. Each list element matches a row in the source dataset
#' and each element in one of the dataframes represents the x/y or lon/lat
#' coordinate pairs of one point of interest.
#' @param profiles Character vector or list. Means of transport as supported by
#' OpenRouteService. For a list of active profiles, call
#' \code{\link[ORSRouting:ORSConfig]{ORSConfig$active_profiles}}. For details
#' on all profiles, refer to the
#' \href{https://giscience.github.io/openrouteservice/documentation/Tag-Filtering.html}{documentation}.
#' @param proximity_type Type of proximity that the calculations should be
#' based on. If `distance`, the shortest physical distance will be calculated
#' and if `duration`, the shortest temporal distance will be calculated.
#' @param port Integer scalar. Port that the local server is running on.
#' @returns Dataframe with distances, travel durations and the index number of
#' the point of interest with the shortest distance to the respective place of
#' the source dataset.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' pois <- query.osm.pois(
#'      datensatz.a,
#'      key = 'amenity',
#'      value = 'hospital',
#'      radius = 5000
#' )
#' shortest_routes <- get_shortest_routes(datensatz.a, pois)
#' shortest_routes
#' #    point_number   route_type poi_number distance duration
#' # 1             1  driving-car          1   7617.6    685.4
#' # 2             1 foot-walking          3   4734.4   3408.7
#' # 3             2  driving-car          4   5050.8    665.1
#' # 4             2 foot-walking          4   4211.4   3032.2
#' # 5             3  driving-car          5   5986.3    635.9
#' # 6             3 foot-walking          4   3931.1   2830.4
#' # 7             4  driving-car         11   4144.4      505
#' # 8             4 foot-walking         11   3587.9   2583.3
#' # 9             5  driving-car         11   1393.1    196.1
#' # 10            5 foot-walking          2    926.9    667.4

get_shortest_routes <- function(
  source,
  poi_coords,
  profiles = c('driving-car', 'foot-walking'),
  proximity_type = 'duration',
  port = 8080
) {
  if (!is.null(dim(poi_coords))) {
    poi_coords <- replicate(
      nrow(source),
      poi_coords,
      simplify = FALSE
    )
  }
  calculate.shortest.routes <- function (profile, point_number) {
    routes <- get_route_lengths(
      replicate(
        nrow(poi_coords[[point_number]]),
        source[point_number, ],
        simplify = FALSE
      ) %>%
        dplyr::bind_rows(),
      poi_coords[[point_number]],
      profile = profile,
      local = TRUE,
      port = port
    )
    # TODO: Implement dataframe conversion for non-nested POI data
    if (tolower(proximity_type) == "distance") {
      best_index <- match(
        min(routes[["distance"]]),
        routes[["distance"]]
      )
    } else if (tolower(proximity_type) == 'duration') {
      best_index <- match(
        min(routes[["duration"]]),
        routes[["duration"]]
      )
    } else {
      cli::cli_abort(
        paste(
          "Expected a proximity type",
          "({.val {\"duration\"}} or {.val {\"distance\"}})"
        )
      )
    }
    best_route <- purrr::map(
      seq_len(ncol(routes)),
      ~routes[best_index, ..1]
    ) %>%
      c(best_index, .)
  }
  nested_iterator <- list(
    profiles = profiles,
    point_number = seq_len(nrow(source))
  ) %>%
    expand.grid()

  route_list <- purrr::pmap(
    nested_iterator,
    ~calculate.shortest.routes(..1, ..2)
  ) %>%
    cbind() %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    cbind(
      nested_iterator[["point_number"]],
      nested_iterator[["profiles"]], .
    )

  output_cols <- c(
    "point_number",
    "route_type",
    "poi_number",
    "distance",
    "duration"
  )
  colnames(route_list) <- output_cols
  return(route_list)
}
