perform_call <- function(req) {
  req <- httr2::req_method(req, "POST")
  req <- httr2::req_error(req, is_error = \(r) FALSE)

  if (is_ors_api(req$url)) {
    req <- httr2::req_throttle(req, rate = 40 / 60)
  }

  res <- httr2::req_perform(req, verbosity = 0L)

  if (req$parse) {
    res <- httr2::resp_body_json(res, simplifyVector = TRUE)
  } else {
    res <- httr2::resp_body_string(res)
  }

  res
}

call_ors_directions <- function(src,
                                dst,
                                profile,
                                units,
                                geometry,
                                params,
                                url,
                                token,
                                parse = TRUE) {
  # Get coordinates in shape
  if (!missing(dst)) {
    locations <- list(as.numeric(src), as.numeric(dst))
  } else {
    locations <- Map(c, src[, 1], src[, 2])
  }

  # Prepare the url
  req <- httr2::request(url)
  req <- httr2::req_url_path(
    req,
    ifelse(!is_ors_api(url), "ors", ""),
    "v2/directions",
    profile,
    ifelse(geometry, "geojson", "")
  )

  req <- httr2::req_headers(
    req,
    Accept = paste(
      "application/json, application/geo+json,",
      "application/gpx+xml, img/png; charset=utf-8"
    ),
    Authorization = if (isTRUE(token)) get_ors_token(),
    `Content-Type` = "application/json; charset=utf-8"
  )

  # Create http body of the request
  body <- list(
    coordinates        = locations,
    alternative_routes = params$alternative_routes,
    attributes         = box(params$attributes),
    continue_straight  = params$continue_straight,
    elevation          = params$elevation,
    extra_info         = box(params$extra_info),
    geometry_simplify  = params$geometry_simplify,
    options            = params$options,
    preference         = params$preference,
    radiuses           = params$radiuses,
    units              = units,
    geometry           = geometry,
    maximum_speed      = params$maximum_speed
  )
  body <- body[lengths(body) > 0L]
  req <- httr2::req_body_json(req, body, digits = NA)
  req$parse <- parse
  perform_call(req)
}



call_ors_matrix <- function(src,
                            dst,
                            profile,
                            metrics,
                            units,
                            url,
                            token) {
  # Format src and dst
  src_list <- unname(split(src, seq_len(nrow(src))))
  src_list <- lapply(src_list, as.numeric)
  dsts_list <- unname(split(dst, seq_len(nrow(dst))))
  dsts_list <- lapply(dsts_list, as.numeric)

  # Coerce dsts and src
  locations <- append(dsts_list, src_list, after = 0L)

  dest_index <- if (nrow(dst) > 1L) {
    seq(nrow(src), length(locations) - 1L)
  } else {
    list(nrow(src))
  }

  src_index <- if (nrow(src) > 1L) {
    seq(0L, nrow(src) - 1L)
  } else {
    list(0L)
  }

  req <- httr2::request(url)
  req <- httr2::req_url_path(
    req,
    ifelse(!is_ors_api(url), "ors", ""),
    "v2/matrix",
    profile
  )

  req <- httr2::req_headers(
    req,
    Accept = "application/json; charset=utf-8",
    Authorization = if (isTRUE(token)) get_ors_token(),
    `Content-Type` = "application/json; charset=utf-8"
  )

  # Create http body of the request
  body <- list(
    locations = locations,
    destinations = dest_index,
    sources = src_index,
    metrics = box(metrics),
    units = units
  )
  req <- httr2::req_body_json(req, body, digits = NA)
  req$parse <- TRUE

  perform_call(req)
}



call_ors_isochrones <- function(src,
                                profile,
                                range,
                                attributes,
                                intersections,
                                interval,
                                location_type,
                                params,
                                range_type,
                                smoothing,
                                area_units,
                                units,
                                url,
                                token) {
  locations <- unname(split(src, seq_len(nrow(src))))
  locations <- lapply(locations, as.numeric)

  req <- httr2::request(url)
  req <- httr2::req_url_path(
    req,
    ifelse(!is_ors_api(url), "ors", ""),
    "v2/isochrones",
    profile,
    "geojson"
  )

  req <- httr2::req_headers(
    req,
    Accept = "application/geo+json; charset=utf-8",
    Authorization = if (isTRUE(token)) get_ors_token(),
    `Content-Type` = "application/json; charset=utf-8"
  )

  body <- list(
    locations = locations,
    range = box(range),
    attributes = box(attributes),
    intersections = intersections,
    interval = interval,
    location_type = location_type,
    options = params,
    range_type = range_type,
    smoothing = smoothing,
    area_units = area_units,
    units = units
  )
  body <- body[lengths(body) > 0L]
  req <- httr2::req_body_json(req, body, digits = NA)
  req$parse <- TRUE

  perform_call(req)
}


call_ors_snap <- function(src, profile, radius, url, ...) {
  locations <- unname(split(src, seq_len(nrow(src))))
  locations <- lapply(locations, as.numeric)

  req <- httr2::request(url)
  req <- httr2::req_url_path(req, "ors/v2/snap", profile, "json")
  req <- httr2::req_headers(
    req,
    Accept = "application/json",
    `Content-Type` = "application/json"
  )

  body <- list(locations = locations, radius = radius, ...)
  req <- httr2::req_body_json(req, body, digits = NA)
  req$parse <- TRUE

  perform_call(req)
}


call_ors_export <- function(bbox, profile, url, ...) {
  bbox <- list(bbox[1:2], bbox[3:4])

  req <- httr2::request(url)
  req <- httr2::req_url_path(req, "ors/v2/export", profile)
  req <- httr2::req_headers(
    req,
    Accept = "application/geo+json",
    `Content-Type` = "application/json"
  )

  body <- list(bbox = bbox, ...)
  req <- httr2::req_body_json(req, body, digits = NA)
  req$parse <- TRUE

  perform_call(req)
}
