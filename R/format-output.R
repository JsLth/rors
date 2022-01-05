# Title     : Response formatting
# Objective : Extract and format information from ORS response
# Created by: Jonas Lieth
# Created on: 22.10.2021


fill_extra_info <- function(values, info_type, profile) {
  fill_fun_name <- paste("fill", info_type, sep = "_")
  if (exists(fill_fun_name)) {
    fill_fun <- match.fun(fill_fun_name)
    if (identical(fill_fun_name, "fill_traildifficulty")) {
      values <- vapply(values, fill_fun, profile, character(1))
    } else {
      values <- vapply(values, fill_fun, character(1))
    }
  }
  values
}


fill_steepness <- function(code) {
  switch(
    as.character(code),
    `-5` = ">16% decline",
    `-4` = "12-15% decline",
    `-3` = "7-11% decline",
    `-2` = "4-6% decline",
    `-1` = "1-3% decline",
    `0`  = "0% incline",
    `1`  = "1-3% incline",
    `2`  = "4-6% incline",
    `3`  = "7-11% incline",
    `4`  = "12-15% incline",
    `5`  = ">16% incline"
  )
}


fill_surface <- function(code) {
  switch(
    as.character(code),
    `0`  = "Unknown",
    `1`  = "Paved",
    `2`  = "Unpaved",
    `3`  = "Asphalt",
    `4`  = "Concrete",
    `5`  = "Cobblestone",
    `6`  = "Metal",
    `7`  = "Wood",
    `8`  = "Compacted Gravel",
    `9`  = "Fine Gravel",
    `10` = "Gravel",
    `11` = "Dirt",
    `12` = "Ground",
    `13` = "Ice",
    `14` = "Paving Stones",
    `15` = "Sand",
    `16` = "Woodchips",
    `17` = "Grass",
    `18` = "Grass Paver"
  )
}


fill_waycategory <- function(code) {
  code <- decode_base2(code)
  cats <- lapply(code, function(c) {
    switch(
      as.character(c),
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
}


fill_waytypes <- function(code) {
  switch(
    as.character(code),
    `0`  = "Unknown",
    `1`  = "State Road",
    `2`  = "Road",
    `3`  = "Street",
    `4`  = "Path",
    `5`  = "Track",
    `6`  = "Cycleway",
    `7`  = "Footway",
    `8`  = "Steps",
    `9`  = "Ferry",
    `10` = "Construction"
  )
}


fill_traildifficulty <- function(code, profile) {
  if (identical(base_profile(profile), "cycling")) {
    code <- code * -1
  }

  switch(
    as.character(code),
    `-7` = "mtb:scale=6",
    `-6` = "mtb:scale=5",
    `-5` = "mtb:scale=4",
    `-4` = "mtb:scale=3",
    `-3` = "mtb:scale=2",
    `-2` = "mtb:scale=1",
    `-1` = "mtb:scale=0",
    `0`  = "No Tag",
    `1`  = "sac_scale=hiking",
    `2`  = "sac_scale=mountain_hiking",
    `3`  = "sac_scale=demanding_mountain_hiking",
    `4`  = "sac_scale=alpine_hiking",
    `5`  = "sac_scale=demanding_alpine_hiking",
    `6`  = "sac_scale=difficult_alpine_hiking"
  )
}


fill_roadaccessrestrictions <- function(code) {
  code <- decode_base2(code)
  cats <- lapply(code, function(c) {
    switch(
      as.character(code),
      `0`  = "None",
      `1`  = "No",
      `2`  = "Customers",
      `4`  = "Destination",
      `8`  = "Delivery",
      `16` = "Private",
      `32` = "Permissive"
    )
  })
  paste(rev(cats), collapse = "/")
}


fill_empty_error_message <- function(code) {
  switch(
    as.character(code),
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
      attributes(error) <- list(error = TRUE)
      return(error)
    }
  } else {
    format <- res$metadata$query$format
    if (identical(format, "geojson")) {
      warnings_geojson <- res$features$properties$warnings
      warnings_json <- res$routes$warnings
      message <- NULL
      code <- NULL
      if (!is.null(warnings_geojson)) {
        message <- lapply(warnings_geojson, function(x) x$message)
        code <- lapply(warnings_geojson, function(x) x$code)
      } else if (!is.null(warnings_json)) {
        message <- lapply(warnings_json, function(x) x$message)
        code <- lapply(warnings_json, function(x) x$code)
      }

      if (length(code) && length(message)) {
        warnings <- lapply(seq_along(code), function(w) {
          paste0("Warning code ", code[w], ": ", message[w])
        })

        if (warn_on_warning) {
          w_vec <- cli::cli_vec(warnings,
                                style = list(vec_sep = "\n", vec_last = "\n"))
          cli::cli_warn(c("ORS returned {length(w_vec)} warning{?s}:", "{w_vec}"))
        } else {
          attributes(warning) <- list(error = FALSE)
          warnings
        }
      }
    }
  }
}

calculate_distances <- function(geometry) {
  distances <- sf::st_length(sf::st_zm(geometry))
  data.frame(distance = round(distances, 2))
}


calculate_avgspeed <- function(distance, duration) {
  speeds <- distance / duration
  units(speeds) <- "km/h"
  data.frame(avgspeed = round(speeds, 2))
}


calculate_durations <- function(res, distances) {
  waypoints <- res$features$properties$segments[[1]]$steps[[1]]$way_points
  wp_distances <- res$features$properties$segments[[1]]$steps[[1]]$distance
  expanded_wp_distances <- expand_by_waypoint(wp_distances, waypoints)
  percentages <- units::drop_units(distances / expanded_wp_distances)
  wp_durations <- res$features$properties$segments[[1]]$steps[[1]]$duration
  durations <- expand_by_waypoint(wp_durations, waypoints) * percentages
  units(durations) <- "s"
  data.frame(duration = round(durations, 2))
}


expand_by_waypoint <- function(vector, waypoints) {
  expand_vctr <- function(i) {
    multiplier <- waypoints[[i]][2] - waypoints[[i]][1]
    rep(vector[i], multiplier)
  }
  expanded_data <- unlist(lapply(seq_len(length(vector)), expand_vctr))
  expanded_data[length(expanded_data) + 1] <- vector[length(vector)]
  expanded_data
}


get_waypoint_index <- function(from, to, waypoints, by_waypoint) {
  if (isFALSE(by_waypoint)) {
    from_index <- match(from + 1, waypoints[[1]])
    to_index <- match(to + 1, waypoints[[2]])
    seq(from_index, to_index)
  }
}


format_extra_info <- function(res, info_type) {
  if (identical(info_type, "waytype")) info_type <- "waytypes"
  waypoints <- res$features$properties$segments[[1]]$steps[[1]]$way_points
  last_waypoint <- res$features$properties$way_points[[1]][2]
  matrix <- res$features$properties$extras[[info_type]]$values[[1]]

  if (length(matrix)) {
    start <- matrix[, 1]
    end <- matrix[, 2]

    iterator <- data.frame(V1 = seq(1, last_waypoint),
                           V2 = seq(1, last_waypoint) + 1)

    indices <- mapply(
      FUN = get_waypoint_index,
      start,
      end,
      MoreArgs = list(waypoints = iterator, by_waypoint = FALSE)
    )

    values <- lapply(seq(1, length(indices)),
                     function(seg) rep(matrix[seg, 3], length(indices[[seg]])))
    values <- unlist(values)

    profile <- res$metadata$query$profile
    values <- fill_extra_info(values, info_type, profile)

    values[length(values) + 1] <- NA
    values_df <- data.frame(values)
  } else {
    values_df <- data.frame(rep(NA, last_waypoint + 1))
  }
  colnames(values_df) <- substitute(info_type)
  values_df
}


extract_ors_attribute <- function(res, attribute) {
  attrib <- res$features$properties$segments[[1]][[attribute]]
  if (!is.null(attrib)) {
    attrib
  }
}


make_summary_table <- function(vector, distances) {
  vector_length <- length(vector)
  total_distance <- sum(distances)
  vector <- as.vector(vector)

  # Determine factor levels
  break_points <- pretty(vector, n = 5, min.n = 1)
  cats <- cut(vector, break_points, include.lowest = TRUE)

  amount_summary <- stats::aggregate(vector, by = list(cats), function(x) {
    length(x) / vector_length * 100
  })

  distance_summary <- lapply(amount_summary$x, function(x) {
    x * total_distance / 100
  })

  data.frame(distance = round(unlist(distance_summary), 1),
             amount = round(amount_summary$x, 1),
             row.names = amount_summary[, 1])
}