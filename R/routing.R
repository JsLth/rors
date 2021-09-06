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


#' Routing between two dataframes
#' @description Calculates the routing distance between two datasets.
#'
#' @param source Source dataset that represents point coordinates that are to
#' be routed from. The source dataset should be passed as a double nested
#' dataframe with each row representing a x/y or lon/lat coordinate
#' pair.
#' @param destination Destination dataset that represents point coordinates
#' that are to be routed to. The source dataset should be passed as a double
#' nested dataframe or list with each row representing a x/y or lon/lat
#' coordinate pair.
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
#' @details If `method = "directions"`, both datasets must have the same shape,
#' i.e. the same number of rows. If `method = "matrix"`, both datasets are
#' allowed to contain only one row. If one of both datasets contains only one
#' row, one-to-many or many-to-one matrices are generated. In general, only
#' one-to-many, many-to-one, one-to-one, or many-to-many combinations are
#' allowed. If you pass a source dataset with 3 rows and a destination dataset
#' with 5 rows, the function cannot know how to match the datasets.
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
  how = "directions",
  local = TRUE,
  port = 8080,
  api_key = NULL,
  geometry = FALSE
) {
  # TODO: Automate method choice
  # (small dataset -> directions, large dataset -> matrix)
  if (how == "directions" && nrow(source) == 1) {
    source <- replicate(
      nrow(destination),
      source,
      simplify = FALSE
    ) %>%
      dplyr::bind_rows()
  }

  source_shape <- cli::cli_vec(
    dim(source),
    style = list(vec_last = ":")
  )
  dest_shape <- cli::cli_vec(
    dim(destination),
    style = list(vec_last = ":")
  )

  if (
    !identical(dim(source), dim(destination)) &&
    how != "matrix"
  ) {
    # For directions calls, both datasets must have the same shape because
    # queries are made row-wise.
    cli::cli_abort(
      c(
        "For directions calls, both datasets must have the same shape.",
        "\nSource dataset shape: {dim(source_shape)}",
        "\nDestination dataset shape: {dim(dest_shape)}"
      )
    )
  } else if (identical(dim(source), dim(destination))) {
    # If both datasets have the same shape, prepare a rowwise iterator for pmap.
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

    if (how == "matrix") {
      cli::cli_warn(
        paste(
          "Row-wise matrix calculations use one-to-one matrices and are",
          "therefore highly inefficient."
        )
      )
    }
  } else if (
    !identical(dim(source), dim(destination)) &&
    !any(sapply(list(source, destination), function(x) nrow(x) == 1))
  ) {
    # If datasets are something like 3 to 6, the function doesn't know which
    # rows to match.
    cli::cli_abort(
      c(
        paste(
          "For matrix calls, the datasets must be either one-to-many or",
          "many-to-many (in the same shape)."
        ),
        "Source dataset shape: {source_shape}",
        "Destination dataset shape: {dest_shape}"
      )
    )
  }

  if (local) {
    url <- paste0("http://localhost:%s/ors/v2/%s/") %>%
      sprintf(port, how)
  } else if (!missing(api_key)) {
    url <- "https://api.openrouteservice.org/v2/%s/" %>%
      sprintf(how)
  } else {
    stop("API key must be passed if queries are not local.")
  }

  extract_lengths.directions <- function(source, dest) {
    route <- query.ors.directions(
      source = source,
      destination = dest,
      profile = profile,
      url = url,
      units = units,
      api_key = api_key,
      geometry = geometry
    )
    return(
      data.frame(
        distance = route$routes[[1]]$summary$distance,
        duration = route$routes[[1]]$summary$duration
      )
    )
  }

  extract_lengths.matrix <- function(source, dest) {
    route <- query.ors.matrix(
      source = source,
      destination = dest,
      profile = profile,
      url = url,
      units = units,
      api_key = api_key
    )
    return(
      data.frame(
        distance = unlist(route$distances[[1]]),
        duration = unlist(route$durations[[1]])
      )
    )
  }

  if (how == "directions") {
    # Routen für jedes Koordinatenpaar berechnen
    route_list <- zipped_locations %>%
      purrr::pmap(extract_lengths.directions) %>%
      dplyr::bind_rows()
  } else if (how == "matrix") {
    if (nrow(source) == 1 || nrow(destination) == 1) {
      route_list <- extract_lengths.matrix(source, destination)
    } else {
      route_list <- zipped_locations %>%
        purrr::pmap(extract_lengths.matrix) %>%
        dplyr::bind_rows()
    }
  }

  return(route_list)
}


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
    auto_unbox = TRUE,
    digits = NA
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

  if (!is.null(routes$error)) {
    cli::cli_abort(
      c(
        "ORS returned the following error:",
        "x" = routes$error$message
      )
    )
  }

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
  if (
    nrow(source) != 1 &&
    nrow(destinations == 1)
  ) {
    # If many-to-one, swap source and destination
    both <- list(source, destinations)
    source <- both[[2]]
    destinations <- both[[1]]
  }

  # Format source and destination
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
    auto_unbox = TRUE,
    digits = NA
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

  if (!is.null(routes$error)) {
    cli::cli_abort(
      c(
        "ORS returned the following error:",
        "x" = paste0(
          routes$error$message,
          " (Code ",
          routes$error$code,
          ")"
        )
      )
    )
  }
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
  source <- adjust_source_data(source)
  poi_coords <- adjust_poi_data(poi_coords, nrow(source))

  calculate.shortest.routes <- function (profile, point_number) {
    routes <- get_route_lengths(
      source[point_number, ],
      poi_coords[[point_number]],
      profile = profile,
      local = TRUE,
      port = port
    )
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


adjust_source_data <- function(source) {
  if (inherits(source, c("list", "matrix"))) {
    as.data.frame(data)
  } else if (inherits(source, c("sf", "sfc"))) {
    reformat_vectordata(data)
  } else if (inherits(source, "data.frame")) {
    source
  } else {
    cli::cli_abort(
      "Source datasets of type {.cls {class(source)}} are not (yet) supported."
    )
  }
}


adjust_poi_data <- function(pois, number_of_points) {
  if (inherits(pois, c("sf", "sfc"))) {
    pois <- reformat_vectordata(pois)
  }
  if (inherits(pois, "list")) {
    elem_type <- lapply(pois, class) %>%
      unique()
    if (length(elem_type) == 1) {
      elem_type <- unlist(elem_type, recursive = FALSE)
      if (any(is.element(elem_type, c("sf", "sfc", "sfg")))) {
        lapply(pois, reformat_vectordata)
      } else if (any(is.element(elem_type, c("matrix", "list")))) {
        lapply(pois, as.data.frame)
      } else if (identical(elem_type, "data.frame")) {
        pois
      } else {
        cli::cli_abort(
      "POI datasets of type {.cls {elem_type}} are not (yet) supported."
        )
      }
    } else {
      multi_types <- lapply(elem_type, paste, collapse = "/") %>%
        cli::cli_vec(
          style = list(
            vec_sep = ", ",
            vec_last = ", ",
            vec_trunc = 8
          )
        )
      cli::cli_abort(
        c(
          "Input POI list contains multiple data types:",
          "{multi_types}",
          "Cannot handle lists with varying data types."
        )
      )
    }
  } else if (inherits(pois, c("data.frame", "matrix"))) {
    as.data.frame(pois)
    replicate(
      number_of_points,
      pois,
      simplify = FALSE
    )
  } else {
    cli::cli_abort(
      "POI datasets of type {.cls {class(pois)}} are not (yet) supported."
    )
  }
}