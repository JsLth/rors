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

get_route_lengths <- function(source,
                              destination,
                              profile,
                              units = "m",
                              geometry = FALSE,
                              options = list()) {
  # Check if ORS is ready to use
  if (!ors_ready(force = FALSE)) {
    cli::cli_abort("ORS service is not reachable.")
  }

  # Bring input data into shape
  source <- format_input_data(source)
  destination <- format_input_data(destination)

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

  extract_lengths <- function(source, dest) {
    route <- query_ors_directions(source = source,
                                  destination = dest,
                                  profile = profile,
                                  units = units,
                                  geometry = geometry,
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



format_input_data <- function(data) {
  if (is.sf(data)) {
    data <- reformat_vectordata(data)[, c("X", "Y")]
  } else {
    if (is.matrix(data) || is.list(data)) {
      data <- as.data.frame(data)
    }
    if (ncol(data) > 2) {
      if (all(is.element(c("X", "Y"), colnames(data)))) {
        data <- data[, c("X", "Y")]
      } else if (all(is.element(c("Lon", "Lat"), colnames(data)))) {
        data <- data[, c("Lon", "Lat")]
      } else if (is.numeric(unlist(data[, c(1, 2)]))) {
        data <- data[, c(1, 2)]
      } else {
        cli::cli_abort(paste("Cannot determine coordinate columns of",
                             "dataframe {.var {deparse(substitute(data))}}"))
      }
    }
  }
  data
}



format_ors_options <- function(options, profile) {
  options_check <- NULL

  if (!is.null(options$avoid_borders) ||
      !is.null(options$avoid_countries) ||
      !is.null(options$avoid_features) ||
      !is.null(options$avoid_polygons) ||
      !is.null(options$profile_params) ||
      !is.null(options$vehicle_type)) {
    if (is.character(options$avoid_borders) &&
        length(options$avoid_borders) == 1 &&
        is.element(options$avoid_borders, c("all", "controlled", "none"))) {
      options_check["avoid_borders"] <- TRUE
    }

    if (is.numeric(options$avoid_countries)) {
      options$avoid_countries <- list(options$avoid_countries)
      options_check["avoid_countries"] <- TRUE
    }

    if (is.character(options$avoid_features)) {
      options$avoid_features <- list(options$avoid_features)
      options_check["avoid_features"] <- TRUE
    }

    if (is.sf(options$avoid_polygons) &&
        sf::st_is(options$avoid_polygons, c("POLYGON", "MULTIPOLYGON"))) {
      geom_type <- as.character(sf::st_geometry_type(options$avoid_polygons))
      options$avoid_polygons <- jsonlite::toJSON(
        list(type = capitalizeChar(geom_type),
             coordinates = list(sf::st_coordinates(options$avoid_polygons))),
        auto_unbox = TRUE
      )
      options_check["avoid_polygons"] <- TRUE
    }

    if (is.list(options$profile_params) && !identical(profile, "driving-car")) {
      raw_profile <- strsplit(profile, "-")[[1]][1]
      allowed_opts <- switch(raw_profile,
                             wheelchair = c("maximum_incline",
                                            "maximum_sloped_kerb",
                                            "minimum_width",
                                            "smoothness_type",
                                            "surface_type",
                                            "track_type"),
                             hgv        = c("axleload",
                                            "hazmat",
                                            "height",
                                            "length",
                                            "weight",
                                            "width"),
                             cycling    = "steepness_difficulty",
                             walking    = c("green", "quiet"))
      opt_admitted <- purrr::map(options$profile_params,
                                 ~is.element(names(.), allowed_opts))
      options_check["profile_param"] <- all(unlist(opt_admitted))

      if (any(opt_admitted$weightings)) {
        weightings <- options$profile_params$weightings[opt_admitted$weightings] %>%
          jsonlite::toJSON(auto_unbox = TRUE)
      } else weightings <- NULL

      if (any(opt_admitted$restrictions)) {
        restrictions <- options$profile_params$restrictions[opt_admitted$restrictions] %>%
          jsonlite::toJSON(auto_unbox = TRUE)
      } else restrictions <- NULL

      profile_param_list <- list(
        weightings = weightings,
        restrictions = restrictions
      )

      profile_param_list[["weightings"]] <- profile_param_list$weightings
      profile_param_list[["restrictions"]] <- profile_param_list$restrictions
      options$profile_params <- jsonlite::toJSON(profile_param_list, auto_unbox = TRUE)
      print(options$profile_params)
    }

    options_list <- list(
      avoid_borders   = options$avoid_borders,
      avoid_countries = options$avoid_countries,
      avoid_features  = options$avoid_features,
      avoid_polygons  = options$avoid_polygons,
      profile_param   = options$profile_params,
      vehicle_type    = options$vehicle_type
    )

    for (element in names(options_list)) {
      options_list[[element]] <- unlist(options_list[[element]],
                                        recursive = FALSE)
    }

    options <- jsonlite::toJSON(options_list, auto_unbox = TRUE)
  } else {
    options <- NULL
  }
  options <- gsub("\\\\", "", options)
  options
}



query_ors_directions <- function(source,
                                 destination,
                                 profile,
                                 units,
                                 geometry,
                                 url) {
  # Get coordinates in shape
  locations <- list(
    c(as.numeric(source[1]), as.numeric(source[2])),
    c(as.numeric(destination[1]), as.numeric(destination[2]))
  )

  # Create http body of the request
  body <- jsonlite::toJSON(
    list(coordinates = locations,
         units = units,
         geometry = geometry),
    auto_unbox = TRUE,
    digits = NA
  )

  # Create request headers
  header <- httr::add_headers(
    Accept = "application/%s; charset=utf-8" %>%
      sprintf(ifelse(geometry, "geo+json", "json")),
    `Content-Type` = "application/json; charset=utf-8"
  )

  # Prepare the url
  url <- httr::modify_url(url = url,
                          path = paste0("ors/v2/directions/",
                                        profile,
                                        if (geometry) "/geojson"))

  # Calculate routes for every profile
  response <- url %>%
    httr::POST(body = body,
               encode = "json",
               header) %>%
    httr::content(as = "text",
                  type = "application/json",
                  encoding = "UTF-8")

  parsed_response <- jsonlite::fromJSON(response)

  if (!is.null(parsed_response$error)) {

    if (is.null(parsed_response$error$message))
      parsed_response$
        error$
        message <- fill_empty_error_message(parsed_response$error$code)

    cli::cli_abort(
      c("ORS returned the following error:",
        "!" = paste0("Code ",
                     parsed_response$error$code,
                     ": ",
                     parsed_response$error$message),
        "i" = get_error_tip(parsed_response$error$code))
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



query_ors_matrix <- function(source,
                             destinations,
                             profile,
                             metrics,
                             units,
                             url) {
  # Format source and destination
  source_list <- dplyr::group_by(source, dplyr::row_number()) %>%
    dplyr::group_split(.keep = FALSE) %>%
    lapply(as.numeric)
  destinations_list <- dplyr::group_by(destinations, dplyr::row_number()) %>%
    dplyr::group_split(.keep = FALSE) %>%
    lapply(as.numeric)

  # Coerce destinations and source
  locations <- append(destinations_list,
                      source_list,
                      after = 0)

  dest_index <- if (nrow(destinations) > 1) {
    seq(nrow(source), length(locations) - 1)
  } else {
    list(nrow(source))
  }

  source_index <- if (nrow(source) > 1) {
    seq(0, nrow(source) - 1)
  } else {
    list(0)
  }

  if (length(metrics) == 1) metrics <- list(metrics)

  # Create http body of the request
  body <- jsonlite::toJSON(
    list(locations = locations,
         destinations = dest_index,
         sources = source_index,
         metrics = metrics,
         units = units),
    auto_unbox = TRUE,
    digits = NA
  )

  # Create request headers
  header <- httr::add_headers(
    Accept = "application/json; charset=utf-8",
    `Content-Type` = "application/json; charset=utf-8"
  )

  # Prepare the url
  url <- httr::modify_url(url = url,
                          path = paste0("ors/v2/matrix/",
                                        profile))

  response <- url %>%
    httr::POST(body = body,
               encode = 'json',
               header) %>%
    httr::content(as = "text",
                  type = "application/json",
                  encoding = "UTF-8")

  parsed_response <- jsonlite::fromJSON(response)

  if (!is.null(parsed_response$error)) {

    cli::cli_abort(
      c("ORS returned the following error:",
        "!" = paste0("Code ",
                     parsed_response$error$code,
                     ": ",
                     parsed_response$error$message),
        "i" = get_error_tip(parsed_response$error$code))
    )

  }
  return(parsed_response)
}

