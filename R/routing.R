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
#' \code{\link{get_}}. For details
#' on all profiles, refer to the
#' \href{https://giscience.github.io/openrouteservice/documentation/Tag-Filtering.html}{documentation}.
#' @param units Distance unit for distance calculations ('m', 'km' or 'mi')
#' @param geometry Specifies whether to return distance measures or geometry
#' features.
#' @param options Ignored. Could be added in the future to add ORS query
#' options, e.g. avoid_polygons or maximum_speed.
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
#' source <- ors_sample(5)
#' dest <- ors_sample(5)
#'
#' rowwise <- get_route_lengths(source, dest, "driving-car")
#' rowwise
#'   distance duration
#' 1  30190.7   1872.4 -> A1 to B1
#' 2  32061.1   2898.6 -> A2 to B2
#' 3  28219.9   1901.5 -> A3 to B3
#' 4 126467.0   5763.3 -> A4 to B4
#' 5 131494.0   5634.1 -> A5 to B5
#'
#' one_to_many <- get_route_lengths(source[1], dest, "driving-car", how = "matrix")
#' one_to_many
#'   distance duration
#' 1 30190.73  1872.42 -> A1 to B1
#' 2 99046.71  4158.36 -> A1 to B2
#' 3 47517.33  2975.49 -> A1 to B3
#' 4 17426.51  1402.07 -> A1 to B4
#' 5  9695.32   822.68 -> A1 to B5
#'

get_route_lengths <- function(
  source,
  destination,
  profile,
  units = "m",
  how = "directions",
  geometry = FALSE,
  options = list()
) {
  # TODO: Automate method choice
  # (small dataset -> directions, large dataset -> matrix)
  if (!ors_ready(force = FALSE)) {
    cli::cli_abort("ORS service is not reachable.")
  }

  if (is.sf(source)) {
    source <- reformat_vectordata(source)[, c("X", "Y")]
  }

  if (is.sf(destination)) {
    destination <- reformat_vectordata(destination)[, c("X", "Y")]
  }

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

  if (geometry && how == "matrix") {
    how <- "directions"
    cli::cli_warn(
      paste(
        "Matrix calls cannot return geometries.",
        "Calculations will be done using the directions service."
      )
    )
  }

  if (
    !identical(nrow(source), nrow(destination)) &&
    how != "matrix"
  ) {
    # For directions calls, both datasets must have the same shape because
    # queries are made row-wise.
    cli::cli_abort(
      c(
        "For directions calls, both datasets must have the same shape.",
        "\nSource dataset rows: {source_shape}",
        "\nDestination dataset rows: {dest_shape}"
      )
    )
  } else if (identical(row(source), row(destination))) {
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
      how <- "directions"
    }
  } else if (
    !identical(nrow(source), nrow(destination)) &&
    !any(sapply(list(source, destination), function(x) nrow(x) == 1))
  ) {
    # If datasets are something like 3 to 6, the function doesn't know which
    # rows to match.
    cli::cli_abort(
      c(
        paste(
          "For many-to-many matrix calls, datasets must have an equal amount",
          "of rows. Otherwise the function cannot know which rows to match."
        ),
        "Source dataset shape: {source_shape}",
        "Destination dataset shape: {dest_shape}"
      )
    )
  }

  port <- get_ors_port()

  extract_lengths.directions <- function(source, dest) {
    route <- query.ors.directions(
      source = source,
      destination = dest,
      profile = profile,
      units = units,
      geometry = geometry,
      port = port
    )
    if (!geometry) {
      return(
        data.frame(
          distance = route$routes$summary$distance,
          duration = route$routes$summary$duration
        )
      )
    } else {
      return(
        data.frame(
          distance = route$features$properties$summary$distance,
          duration = route$features$properties$summary$duration,
          geometry = route$features$geometry
        )
      )
    }
  }

  extract_lengths.matrix <- function(source, dest) {
    route <- query.ors.matrix(
      source = source,
      destination = dest,
      profile = profile,
      units = units,
      port = port
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

  if (is.null(route_list$geometry)) {
    return(route_list)
  } else {
    return(sf::st_as_sf(route_list))
  }
}


query.ors.directions <- function(
  source,
  destination,
  profile,
  url,
  units,
  geometry,
  port
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
    Accept = "application/%s; charset=utf-8" %>%
      sprintf(
        ifelse(geometry, "geo+json", "json")
      ),
    `Content-Type` = "application/json; charset=utf-8"
  )

  url <- httr::modify_url(
    sprintf("http://localhost:%s/", port),
    path = paste0("ors/v2/directions/", profile, if (geometry) "/geojson")
  )

  # Calculate routes for every profile
  response <- url %>%
    httr::POST(
      body = body,
      encode = "json",
      header
    ) %>%
    httr::content(
      as = "text",
      type = "application/json",
      encoding = "UTF-8"
    )
  parsed_response <- jsonlite::fromJSON(response)

  if (!is.null(parsed_response$error)) {

    if (is.null(parsed_response$error$message))
      parsed_response$
        error$
        message <- fill_empty_error_message(parsed_response$error$code)

    cli::cli_abort(
      c(
        "ORS returned the following error:",
        "!" = paste0(
          "Code ",
          parsed_response$error$code,
          ": ",
          parsed_response$error$message
        ),
        "i" = get_error_tip(parsed_response$error$code)
      )
    )
  }

  if (!geometry) {
    return(parsed_response)
  } else {
    parsed_response$
      features$
      geometry <- sf::st_multilinestring(
      parsed_response$
        features$
        geometry$
        coordinates
    ) %>% sf::st_sfc()
    return(parsed_response)
  }
}


query.ors.matrix <- function(
  source,
  destinations,
  profile,
  units,
  port
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
      destinations = if (length(locations) > 2) {
        seq(length(locations) - 1)
      } else {
        list(1)
      },
      sources = list(0),
      metrics = list("distance", "duration"),
      units = units
    ),
    auto_unbox = TRUE,
    digits = NA
  )
    header <- httr::add_headers(
    Accept = "application/json; charset=utf-8",
    `Content-Type` = "application/json; charset=utf-8"
  )

  url <- httr::modify_url(
    sprintf("http://localhost:%s/", port),
    path = paste0("ors/v2/matrix/", profile)
  )
  response <- url %>%
    httr::POST(
      body = body,
      encode = 'json',
      header
    ) %>%
    httr::content(
      as = "text",
      type = "application/json",
      encoding = "UTF-8"
    )
  parsed_response <- jsonlite::fromJSON(response)

  if (!is.null(parsed_response$error)) {

    cli::cli_abort(
      c(
        "ORS returned the following error:",
        "!" = paste0(
          "Code ",
          parsed_response$error$code,
          ": ",
          parsed_response$error$message
        ),
        "i" = get_error_tip(parsed_response$error$code)
      )
    )
  }
  return(parsed_response)
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
#' @param poi_coords Dataset containing points of interest that should be
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


get_shortest_routes <- function(
  source,
  pois,
  profiles = get_profiles(),
  proximity_type = 'duration',
  ...
) {
  source <- adjust_source_data(source)
  pois <- adjust_poi_data(pois, nrow(source))


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
    best_route <- routes[best_index,] %>%
      cbind(best_index, .)
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
    "duration",
    if (geometry) "geometry"
  )
  colnames(route_list) <- output_cols
  rownames(route_list) <- NULL

  if (!geometry) {
    return(route_list)
  } else {
    return(sf::st_as_sf(route_list))
  }

}


adjust_source_data <- function(source) {
  if (is.list(source) || is.matrix(source)) {
    as.data.frame(source)
  } else if (is.sf(source)) {
    reformat_vectordata(source)
  } else if (is.data.frame(source)) {
    source
  } else {
    cli::cli_abort(
      "Source datasets of type {.cls {class(source)}} are not (yet) supported."
    )
  }
}


adjust_poi_data <- function(pois, number_of_points) {
  if (is.sf(pois)) {
    pois <- reformat_vectordata(pois)
  }
  if (is.list(pois)) {
    elem_type <- lapply(pois, class) %>%
      unique()
    if (length(elem_type) == 1) {
      elem_type <- unlist(elem_type, recursive = FALSE)
      if (any(is.element(elem_type, c("sf", "sfc")))) {
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
  } else if (is.data.frame(pois) || is.matrix(pois)) {
    as.data.frame(pois)
    pois <- pois[, c(1,2)]
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