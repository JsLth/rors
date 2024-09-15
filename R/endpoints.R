perform_call <- function(req) {
  req <- httr2::req_user_agent(req, "https://github.com/jslth/rors")
  req <- httr2::req_error(req, is_error = function(x) {
    !grepl("application/json", x$headers$`Content-Type`, fixed = TRUE)
  })

  if (isTRUE(getOption("rors_echo"))) {
    message(capture.output(req))
  }

  res <- httr2::req_perform(req, verbosity = 0L)

  if (isFALSE(req$parse)) {
    res <- httr2::resp_body_string(res)
  } else {
    res <- httr2::resp_body_json(res, simplifyVector = TRUE)
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

  if (!is_ors_api(url)) {
    req <- httr2::req_url_path(req, "ors")
  }
  format <- ifelse(geometry, "geojson", "json")
  req <- httr2::req_template(req, "POST v2/directions/{profile}/{format}")
  req <- set_headers(req, token)

  # Create http body of the request
  body <- c(
    coordinates = list(locations),
    params,
    units = units,
    geometry = geometry
  )
  body$attributes <- box(body$attributes)
  body$extra_info <- box(body$extra_info)
  body <- body[lengths(body) > 0L]

  req <- httr2::req_body_json(req, body, digits = NA)
  throttle <- getOption("rors_throttle_directions", 40 / 60)
  req <- set_rate_limits(req, throttle)
  req$parse <- parse
  perform_call(req)
}



call_ors_matrix <- function(src,
                            dst,
                            profile,
                            metrics,
                            units,
                            resolve_locations,
                            url,
                            token) {
  # Format src and dst
  src_list <- unname(split(src, seq_len(nrow(src))))
  src_list <- lapply(src_list, as.numeric)
  dsts_list <- unname(split(dst, seq_len(nrow(dst))))
  dsts_list <- lapply(dsts_list, as.numeric)

  # Coerce dsts and src
  locations <- append(dsts_list, src_list, after = 0)

  dest_index <- if (nrow(dst) > 1) {
    seq(nrow(src), length(locations) - 1)
  } else {
    list(nrow(src))
  }

  src_index <- if (nrow(src) > 1) {
    seq(0, nrow(src) - 1)
  } else {
    list(0)
  }

  req <- httr2::request(url)

  if (!is_ors_api(url)) {
    req <- httr2::req_url_path(req, "ors")
  }
  req <- httr2::req_template(req, "POST v2/matrix/{profile}")
  req <- set_headers(req, token)

  # Create http body of the request
  body <- list(
    locations = locations,
    destinations = dest_index,
    sources = src_index,
    metrics = box(metrics),
    units = units,
    resolve_locations = resolve_locations
  )
  req <- httr2::req_body_json(req, body, digits = NA)
  throttle <- getOption("rors_throttle_matrix", 40 / 60)
  req <- set_rate_limits(req, throttle)
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

  if (!is_ors_api(url)) {
    req <- httr2::req_url_path(req, "ors")
  }
  req <- httr2::req_template(req, "POST v2/isochrones/{profile}/geojson")
  req <- set_headers(req, token)

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
  throttle <- getOption("rors_throttle_isochrones", 20 / 60)
  req <- set_rate_limits(req, throttle)
  req$parse <- TRUE

  perform_call(req)
}


call_ors_snap <- function(src, profile, radius, url, token, ...) {
  locations <- unname(split(src, seq_len(nrow(src))))
  locations <- lapply(locations, as.numeric)

  req <- httr2::request(url)
  if (!is_ors_api(url)) {
    req <- httr2::req_url_path(req, "ors")
  }
  req <- httr2::req_template(req, "POST v2/snap/{profile}/json")
  req <- set_headers(req, token)

  body <- list(locations = locations, radius = radius, ...)
  req <- httr2::req_body_json(req, body, digits = NA)
  throttle <- getOption("rors_throttle_snap", 100 / 60)
  req <- set_rate_limits(req, throttle)
  req$parse <- TRUE

  perform_call(req)
}


call_ors_export <- function(bbox, profile, url, ...) {
  bbox <- list(bbox[1:2], bbox[3:4])

  req <- httr2::request(url)
  req <- httr2::req_template(req, "ors/v2/export/{profile}")
  req <- set_headers(req, token)

  body <- list(bbox = bbox, ...)
  req <- httr2::req_body_json(req, body, digits = NA)
  req$parse <- TRUE

  perform_call(req)
}


set_rate_limits <- function(req, throttle) {
  if (is_ors_api(req$url)) {
    retries <- getOption("rors_retries", 3)
    req <- httr2::req_retry(req, max_tries = retries)
    req <- httr2::req_throttle(req, rate = throttle)
  }
  req
}


set_headers <- function(req, token) {
  httr2::req_headers(
    req,
    Accept = "application/json, application/geo+json,",
    Authorization = if (isTRUE(token)) get_ors_token(),
    `Content-Type` = "application/json; charset=utf-8"
  )
}
