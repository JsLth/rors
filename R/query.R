# Title     : Query ORS
# Objective : Make queries to the directions or matrix API from ORS
# Created by: Jonas Lieth
# Created on: 14.10.2021


perform_query <- function(url, body, header) {
  for (i in seq(1, 2)) {
    res_object <- httr::POST(url, body = body, encode = "json", header)
    res_text <- httr::content(
      x = res_object,
      as = "text",
      type = "application/json",
      encoding = "UTF-8"
    )

    res <- jsonlite::fromJSON(res_text)
    if (identical(res$error, "Rate limit exceeded")) {
      cli::cli_alert_info("Waiting out rate limit... (60s)")
      Sys.sleep(60)
      next
    } else break
  }

  res
}



query_ors_directions <- function(
  source,
  destination,
  profile,
  units,
  geometry,
  options,
  url,
  token
  ) {
  # Get coordinates in shape
  locations <- list(
    c(as.numeric(source[1L]), as.numeric(source[2L])),
    c(as.numeric(destination[1L]), as.numeric(destination[2L]))
  )

  # Create http body of the request
  body_list <- list(
    coordinates       = locations,
    attributes        = box(options$attributes),
    continue_straight = options$continue_straight,
    elevation         = options$elevation,
    extra_info        = box(options$extra_info),
    geometry_simplify = options$geometry_simplify,
    options           = options$options,
    preference        = options$preference,
    radiuses          = options$radiuses,
    units             = units,
    geometry          = geometry,
    maximum_speed     = options$maximum_speed
  )
  body_list <- body_list[lengths(body_list) > 0L]
  body <- jsonlite::toJSON(body_list, auto_unbox = TRUE, digits = NA)

  # Create request headers
  header <- httr::add_headers(
    Accept = sprintf(
      "application/%s; charset=utf-8",
      ifelse(geometry, yes = "geo+json", no = "json")
    ),
    Authorization = token,
    `Content-Type` = "application/json; charset=utf-8"
  )

  # Prepare the url
  path <- paste0(
    if (is_ors_api(url)) "" else "ors",
    "/v2/directions/",
    profile,
    if (geometry) "/geojson"
  )
  url <- httr::modify_url(url = url, path = path)

  # Calculate routes for every profile
  perform_query(url, body, header)
}



query_ors_matrix <- function(
  source,
  destination,
  profile,
  metrics,
  units,
  url
) {
  # Format source and destination
  source_list <- unname(split(source, seq_len(nrow(source))))
  source_list <- lapply(source_list, as.numeric)
  destinations_list <- unname(split(destination, seq_len(nrow(destination))))
  destinations_list <- lapply(destinations_list, as.numeric)

  # Coerce destinations and source
  locations <- append(destinations_list, source_list, after = 0L)

  dest_index <- if (nrow(destination) > 1L) {
    seq(nrow(source), length(locations) - 1L)
  } else {
    list(nrow(source))
  }

  source_index <- if (nrow(source) > 1L) {
    seq(0L, nrow(source) - 1L)
  } else {
    list(0L)
  }

  # Create http body of the request
  body_list <- list(
    locations = locations,
    destinations = dest_index,
    sources = source_index,
    metrics = box(metrics),
    units = units
  )
  
  body <- jsonlite::toJSON(body_list, auto_unbox = TRUE, digits = NA)

  # Create request headers
  header <- httr::add_headers(
    Accept = "application/json; charset=utf-8",
    Authorization = token,
    `Content-Type` = "application/json; charset=utf-8"
  )

  # Prepare the url
  url <- httr::modify_url(
    url = url,
    path = paste0("ors/v2/matrix/", profile)
  )

  perform_query(url, body, header)
}



query_isochrone <- function(
  source,
  profile,
  range,
  attributes,
  intersections,
  interval,
  location_type,
  options,
  range_type,
  smoothing,
  area_units,
  units,
  url,
  token
) {
  locations <- unname(split(source, seq_len(nrow(source))))
  locations <- lapply(locations, as.numeric)
  
  body_list <- list(
    locations = locations,
    range = box(range),
    attributes = box(attributes),
    intersections = intersections,
    interval = interval,
    location_type = location_type,
    options = options,
    range_type = range_type,
    smoothing = smoothing,
    area_units = area_units,
    units = units
  )
  
  body <- jsonlite::toJSON(body_list, auto_unbox = TRUE, digits = NA)
  
  # Create request headers
  header <- httr::add_headers(
    Accept = "application/geo+json; charset=utf-8",
    Authorization = token,
    `Content-Type` = "application/json; charset=utf-8"
  )
  
  # Prepare the url
  url <- httr::modify_url(
    url = url,
    path = sprintf("ors/v2/isochrones/%s/geojson", profile)
  )
  
  perform_query(url, body, header)
}
