perform_call <- function(req) {
  req <- httr2::req_method(req, "POST")
  req <- httr2::req_error(req, is_error = \(e) FALSE)

  if (is_ors_api(req$url)) {
    req <- httr2::req_throttle(req, rate = 40 / 60)
  }

  res <- httr2::req_perform(req, verbosity = 0L)
  httr2::resp_body_json(res, simplifyVector = TRUE)
}

call_ors_directions <- function(source,
                                 destination,
                                 profile,
                                 units,
                                 geometry,
                                 options,
                                 url,
                                 token) {
  # Get coordinates in shape
  locations <- list(
    c(as.numeric(source[1L]), as.numeric(source[2L])),
    c(as.numeric(destination[1L]), as.numeric(destination[2L]))
  )

  # Prepare the url
  req <- httr2::request(url)
  req <- httr2::req_url_path(
    req,
    ifelse(!is_ors_api(url), "ors", ""),
    "v2/directions",
    profile,
    ifelse(geometry, "/geojson", "")
  )

  req <- httr2::req_headers(
    req,
    Accept = sprintf(
      "application/%s; charset=utf-8",
      ifelse(geometry, "geo+json", "json")
    ),
    Authorization = token,
    `Content-Type` = "application/json, application/geo+json, charset=utf-8"
  )

  # Create http body of the request
  body <- list(
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
  body <- body[lengths(body) > 0L]
  req <- httr2::req_body_json(req, body, digits = NA)

  perform_call(req)
}



call_ors_matrix <- function(source,
                             destination,
                             profile,
                             metrics,
                             units,
                             url,
                             token) {
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

  req <- httr2::request(url)
  req <- httr2::req_url_path(req, "ors/v2/matrix", profile)

  req <- httr2::req_headers(
    req,
    Accept = "application/json; charset=utf-8",
    Authorization = token,
    `Content-Type` = "application/json; charset=utf-8"
  )

  # Create http body of the request
  body_list <- list(
    locations = locations,
    destinations = dest_index,
    sources = source_index,
    metrics = box(metrics),
    units = units
  )
  req <- httr2::req_body_json(req, body_list, digits = NA, null = "list")

  perform_call(req)
}



call_ors_isochrones <- function(source,
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
                            token) {
  locations <- unname(split(source, seq_len(nrow(source))))
  locations <- lapply(locations, as.numeric)

  req <- httr2::request(url)
  req <- httr2::req_url_path(req, "ors/v2/isochrones", profile, "geojson")

  req <- httr2::req_headers(
    req,
    Accept = "application/geo+json; charset=utf-8",
    Authorization = token,
    `Content-Type` = "application/json; charset=utf-8"
  )

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
  req <- httr2::req_body_json(req, body_list, digits = NA)

  perform_call(req)
}
