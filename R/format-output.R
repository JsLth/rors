#' Gets and extracts distance and durations values from a Directions request.
#' @param index Row index of input dataset
#' @param env Environment containing all parameters necessary to query ORS
#' @noRd
extract_summary <- function(index, env) {
  call_index <- env$call_index

  res <- query_ors_directions(
    source = env$locations[index, "source"],
    destination = env$locations[index, "dest"],
    profile = env$profile,
    units = env$units,
    geometry = env$geometry,
    options = env$options,
    url = env$url,
    token = env$instance$token
  )

  cond <- handle_ors_conditions(res)

  if (isTRUE(attr(cond, "error"))) {
    ors_cache$routing_conditions[[call_index]][index] <- cond

    if (!env$geometry) {
      return(data.frame(distance = NA, duration = NA))
    } else {
      empty_line <- sf::st_sfc(sf::st_linestring(), crs = 4326L)
      return(sf::st_sf(distance = NA, duration = NA, geometry = empty_line))
    }
  } else if (isFALSE(attr(cond, "error"))) {
    ors_cache$routing_conditions[[call_index]][index] <- unlist(cond)
  } else {
    ors_cache$routing_conditions[[call_index]][index] <- NA
  }

  if (!env$geometry) {
    distance <- res$routes$summary$distance
    duration <- res$routes$summary$duration
    if (is.null(distance)) distance <- 0
    if (is.null(duration)) duration <- 0
    data.frame(
      distance = distance,
      duration = duration
    )
  } else {
    distance <- res$features$properties$summary$distance
    duration <- res$features$properties$summary$duration
    if (is.null(distance)) distance <- 0
    if (is.null(duration)) duration <- 0
    linestring <- sf::st_linestring(res$features$geometry$coordinates[[1L]])
    sf::st_sf(
      distance = distance,
      duration = duration,
      geometry = sf::st_sfc(linestring, crs = 4326L)
    )
  }
}



#' Dispenser function to replace response values with more informative ones
#' @param values Object from the response list
#' @param info_type Type of information to be replaced
#' @noRd
fill_extra_info <- function(values, info_type, ...) {
  fill_fun_name <- paste("fill", info_type, sep = "_")
  if (exists(fill_fun_name)) {
    fill_fun <- match.fun(fill_fun_name)
    values <- fill_fun(values, ...)
  }
  values
}


fill_steepness <- function(codes, ...) {
  levels <- seq(-5, 5)
  labels <- c(
    ">16% decline", "12-15% decline", "7-11% decline", "4-6% decline",
    "1-3% decline", "0% incline", "1-3% incline", "4-6% incline",
    "7-11% incline", "12-15% incline", ">16% incline"
  )

  ordered(codes, levels = levels, labels = labels)
}


fill_surface <- function(codes, ...) {
  levels <- seq(0, 18)
  labels <- c(
    "Unknown", "Paved", "Unpaved", "Asphalt", "Concrete", "Cobblestone",
    "Metal", "Wood", "Compacted Gravel", "Fine Gravel", "Gravel", "Dirt",
    "Ground", "Ice", "Paving Stones", "Sand", "Woodchips", "Grass", "Grass Paver"
  )

  factor(codes, levels, labels = labels)
}


fill_waycategory <- function(codes, ...) {
  cats <- vapply(codes, function(code) {
    code <- decode_base2(code)
    cats <- lapply(code, function(c) {
      switch(as.character(c),
        `0`   = "No category",
        `1`   = "Highway",
        `2`   = "Steps",
        `4`   = "Unpaved Road",
        `8`   = "Ferry",
        `16`  = "Track",
        `32`  = "Tunnel",
        `64`  = "Paved road",
        `128` = "Ford"
      )
    })
    paste(rev(cats), collapse = "/")
  }, character(1))

  as.factor(cats)
}


fill_waytypes <- function(codes, ...) {
  levels <- seq(0, 10)
  labels <- c(
    "Unknown", "State Road", "Road", "Street", "Path", "Track", "Cycleway",
    "Footway", "Steps", "Ferry", "Construction"
  )

  factor(codes, levels = levels, labels = labels)
}


fill_traildifficulty <- function(codes, profile) {
  if (base_profile(profile) == "cycling") {
    codes <- codes * -1L
    levels <- seq(-7, 0)
    labels <- c(
      "mtb:scale=6", "mtb:scale=5", "mtb:scale=4", "mtb:scale=3", "mtb:scale=2",
      "mtb:scale=1", "mtb:scale=0", "No Tag"
    )
  } else {
    levels <- seq(0, 6)
    labels <- c(
      "No Tag", "sac_scale=hiking", "sac_scale=mountain_hiking",
      "sac_scale=demanding_mountain_hiking", "sac_scale=alpine_hiking",
      "sac_scale=demanding_alpine_hiking", "sac_scale=difficult_alpine_hiking"
    )
  }

  ordered(codes, levels = levels, labels = labels)
}


fill_roadaccessrestrictions <- function(codes, ...) {
  cats <- vapply(codes, function(code) {
    code <- decode_base2(code)
    cat <- lapply(code, function(c) {
      switch(as.character(c),
        `0`  = "None",
        `1`  = "No",
        `2`  = "Customers",
        `4`  = "Destination",
        `8`  = "Delivery",
        `16` = "Private",
        `32` = "Permissive"
      )
    })
    paste(rev(cat), collapse = "/")
  }, character(1))

  as.factor(cats)
}


#' Replaces empty error message strings based on their error code
#' @noRd
fill_empty_error_message <- function(code) {
  switch(as.character(code),
    `2000` = "Unable to parse JSON request.",
    `2001` = "Required parameter is missing.",
    `2002` = "Invalid parameter format.",
    `2003` = "Invalid parameter value.",
    `2004` = "Parameter value exceeds the maximum allowed limit.",
    `2006` = "Unable to parse the request to the export handler.",
    `2007` = "Unsupported export format.",
    `2008` = "Empty Element.",
    `2009` = "Route could not be found between locations.",
    `2099` = "Unknown internal error.",
    `6000` = "Unable to parse JSON request.",
    `6001` = "Required parameter is missing.",
    `6002` = "Invalid parameter format.",
    `6003` = "Invalid parameter value.",
    `6004` = "Parameter value exceeds the maximum allowed limit.",
    `6006` = "Unable to parse the request to the export handler.",
    `6007` = "Unsupported export format.",
    `6008` = "Empty Element.",
    `6099` = "Unknown internal error."
  )
}



#' Accepts a result list and handles error and warning codes
#' @param res Response list from `query_ors_directions`
#' @param abort_on_error Whether to abort when an error code is returned
#' @param warn_on_warning Whether to warn when a warning code is returned
#' @noRd
handle_ors_conditions <- function(res, abort_on_error = FALSE, warn_on_warning = FALSE) {
  if (!is.null(res$error)) {
    message <- res$error$message
    code <- res$error$code
    if (is.null(res$error$message)) {
      message <- fill_empty_error_message(res$error$code)
    }
    error <- paste0("Error code ", code, ": ", message)
    if (abort_on_error) {
      cli::cli_abort(c("ORS encountered the following exception:", error))
    } else {
      structure(error, error = TRUE)
    }
  } else {
    format <- res$metadata$query$format
    warnings_geojson <- res$features$properties$warnings[[1L]]
    warnings_json <- res$routes$warnings[[1L]]
    message <- NULL
    code <- NULL
    if (!is.null(warnings_geojson)) {
      message <- warnings_geojson$message
      code <- warnings_geojson$code
    } else if (!is.null(warnings_json)) {
      message <- warnings_json$message
      code <- warnings_json$code
    }

    if (length(code) && length(message)) {
      warnings <- lapply(seq_along(code), function(w) {
        paste0("Warning code ", code[w], ": ", message[w])
      })

      if (warn_on_warning) {
        w_vec <- cli::cli_vec(
          warnings,
          style = list(vec_sep = "\f", vec_last = "\f")
        )
        cli::cli_warn(c("ORS returned {length(w_vec)} warning{?s}:", w_vec))
      } else {
        structure(warnings, error = FALSE)
      }
    }
  }
}

calculate_distances <- function(geometry) {
  distances <- sf::st_length(sf::st_zm(geometry))
  data.frame(distance = round(distances, 2L))
}


calculate_avgspeed <- function(distance, duration) {
  speeds <- distance / duration
  units(speeds) <- "km/h"
  data.frame(avgspeed = round(speeds, 2L))
}


calculate_durations <- function(res, distances) {
  waypoints <- res$features$properties$segments[[1L]]$steps[[1L]]$way_points
  wp_distances <- res$features$properties$segments[[1L]]$steps[[1L]]$distance
  expanded_wp_distances <- expand_by_waypoint(wp_distances, waypoints)
  percentages <- units::drop_units(distances / expanded_wp_distances)
  wp_durations <- res$features$properties$segments[[1L]]$steps[[1L]]$duration
  durations <- expand_by_waypoint(wp_durations, waypoints) * percentages
  units(durations) <- "s"
  data.frame(duration = round(durations, 2L))
}


expand_by_waypoint <- function(vector, waypoints) {
  expand_vctr <- function(i) {
    multiplier <- waypoints[[i]][2L] - waypoints[[i]][1L]
    rep(vector[i], multiplier)
  }
  expanded_data <- unlist(lapply(seq_len(length(vector)), expand_vctr))
  expanded_data[length(expanded_data) + 1L] <- vector[length(vector)]
  expanded_data
}


get_waypoint_index <- function(from, to, waypoints, by_waypoint) {
  if (isFALSE(by_waypoint)) {
    from_index <- match(from + 1L, waypoints[[1L]])
    to_index <- match(to + 1L, waypoints[[2L]])
    seq(from_index, to_index)
  }
}


format_extra_info <- function(res, info_type) {
  if (identical(info_type, "waytype")) info_type <- "waytypes"
  last_waypoint <- res$features$properties$way_points[[1L]][2L]
  matrix <- res$features$properties$extras[[info_type]]$values[[1L]]

  if (length(matrix)) {
    start <- matrix[, 1L]
    end <- matrix[, 2L]

    iterator <- data.frame(
      V1 = seq(1L, last_waypoint),
      V2 = seq(1L, last_waypoint) + 1L
    )

    indices <- mapply(
      FUN = get_waypoint_index,
      start,
      end,
      MoreArgs = list(waypoints = iterator, by_waypoint = FALSE)
    )

    if (is.matrix(indices)) indices <- list(c(indices))

    values <- lapply(
      seq(1, length(indices)),
      function(seg) rep(matrix[seg, 3L], length(indices[[seg]]))
    )
    values <- unlist(values)

    profile <- res$metadata$query$profile
    values <- fill_extra_info(values, info_type, profile)

    values[length(values) + 1L] <- NA
    values_df <- data.frame(values)
  } else {
    values_df <- data.frame(rep(NA, last_waypoint + 1L))
  }

  colnames(values_df) <- info_type
  
  values_df
}


extract_ors_attribute <- function(res, attribute) {
  attrib <- res$features$properties$segments[[1L]][[attribute]]
  if (!is.null(attrib)) {
    attrib
  }
}


make_summary_table <- function(vector, distances) {
  vector_length <- length(vector)
  total_distance <- sum(distances)
  vector <- as.vector(vector)

  # Determine factor levels
  break_points <- pretty(vector, n = 5L, min.n = 1L)
  cats <- cut(vector, break_points, include.lowest = TRUE)

  amount_summary <- stats::aggregate(vector, by = list(cats), function(x) {
    length(x) / vector_length * 100L
  })

  distance_summary <- lapply(amount_summary$x, function(x) {
    x * total_distance / 100L
  })

  data.frame(
    distance = round(unlist(distance_summary), 1L),
    amount = round(amount_summary$x, 1L),
    row.names = amount_summary[, 1L]
  )
}
