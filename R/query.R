# Title     : Query ORS
# Objective : Make queries to the directions or matrix API from ORS
# Created by: Jonas Lieth
# Created on: 14.10.2021


query_ors_directions <- function(source,
                                 destination,
                                 profile,
                                 units,
                                 geometry,
                                 options,
                                 url) {
  # Get coordinates in shape
  locations <- list(
    c(as.numeric(source[1]), as.numeric(source[2])),
    c(as.numeric(destination[1]), as.numeric(destination[2]))
  )

  # Create http body of the request
  body_list <- list(coordinates       = locations,
                    attributes        = options$attributes,
                    continue_straight = options$continue_straight,
                    elevation         = options$elevation,
                    extra_info        = options$extra_info,
                    geometry_simplify = options$geometry_simplify,
                    options           = options$options,
                    preference        = options$preference,
                    radiuses          = options$radiuses,
                    units             = units,
                    geometry          = geometry,
                    maximum_speed     = options$maximum_speed)
  body_list <- body_list[lengths(body_list) > 0]
  body <- jsonlite::toJSON(body_list, auto_unbox = TRUE, digits = NA)

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

    cli::cli_abort(c("ORS returned the following error:",
                     "!" = paste0("Code ",
                                  parsed_response$error$code,
                                  ": ",
                                  parsed_response$error$message),
                     "i" = get_error_tip(parsed_response$error$code)))
  } # TODO: Warning instead of error
  # TODO: Function to show last ORS errors

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

    if (is.null(parsed_response$error$message))
      parsed_response$
        error$
        message <- fill_empty_error_message(parsed_response$error$code)

    cli::cli_abort(c("ORS returned the following error:",
                     "!" = paste0("Code ",
                                  parsed_response$error$code,
                                  ": ",
                                  parsed_response$error$message),
                     "i" = get_error_tip(parsed_response$error$code)))
  }
  return(parsed_response)
}
