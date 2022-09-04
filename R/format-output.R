#' Gets and extracts distance and durations values from a Directions request.
#' @param index Row index of input dataset
#' @param env Environment containing all parameters necessary to query ORS
#' @noRd
apply_directions <- function(index,
                             locations,
                             profile,
                             units,
                             geometry,
                             options,
                             url,
                             instance,
                             call_index) {
  res <- call_ors_directions(
    source = locations[index, "source"],
    destination = locations[index, "dest"],
    profile = profile,
    units = units,
    geometry = geometry,
    options = options,
    url = url,
    token = instance$token
  )

  cond <- handle_ors_conditions(res)
  store_condition(cond, call_index, index)
  get_ors_summary(res, geometry = geometry)
}



apply_shortest_routes <- function(index,
                                  source,
                                  destination,
                                  iter,
                                  units,
                                  geometry,
                                  instance,
                                  type,
                                  ...) {
  routes <- suppressWarnings(
    ors_distances(
      source = source[iter[index, "point_number"], ],
      destination = if (is.data.frame(destination)) {
        destination
      } else if (is.list(destination)) {
        destination[[iter[index, "point_number"]]]
      },
      profile = iter[index, "profile"],
      units = units,
      geometry = geometry,
      instance = instance,
      ...
    )
  )
  
  best_index <- suppressWarnings(
    match(min(routes[[type]], na.rm = TRUE), routes[[type]])
  )

  cbind(
    dest = best_index,
    routes[best_index, ],
    has_error = anyNA(routes)
  )
}



#' Replace response values with more informative ones
#' @param values Object from the response list
#' @param info_type Type of information to be replaced
#' @noRd
fill_extra_info <- function(codes, info_type, profile) {
  tab <- fill_table[fill_table$name %in% info_type, ]
  
  if (nrow(tab)) {
    profile <- evalq(tab$profile)
    fct_fun <- ifelse(tab$ordinal, ordered, factor)
    
    if (tab$base2) {
      cats <- vapply(codes, function(code) {
        decodes <- decode_base2(code)
        cats <- lapply(decodes, function(d) {
          tab[tab$levels %in% d, "labels"]
        })
        paste(rev(cats), collapse = "/")
      }, character(1))
    }

    fct_fun(codes, labels = unlist(tab$labels), levels = unlist(tab$levels))
  } else {
    codes
  }
}


fill_table <- tibble::tibble(
  name = c(
    "steepness", "surface", "waycategory", "waytypes",
    "traildifficulty", "roadaccessrestrictions"
  ),
  levels = list(
    seq(-5, 5), seq(0, 18), c(0, 1, 2, 4, 8, 16, 32, 64, 128),
    seq(0, 10), seq(-7, 6), c(0, 1, 2, 4, 8, 16, 32)
  ),
  labels = list(
    c(
      ">16% decline", "12-15% decline", "7-11% decline", "4-6% decline",
      "1-3% decline", "0% incline", "1-3% incline", "4-6% incline",
      "7-11% incline", "12-15% incline", ">16% incline"
    ),
    c(
      "Unknown", "Paved", "Unpaved", "Asphalt", "Concrete", "Cobblestone",
      "Metal", "Wood", "Compacted Gravel", "Fine Gravel", "Gravel", "Dirt",
      "Ground", "Ice", "Paving Stones", "Sand", "Woodchips", "Grass", "Grass Paver"
    ),
    c(
      "No category", "Highway", "Steps", "Unpaved Road", "Ferry", "Track",
      "Tunnel", "Paved Road", "Ford"
    ),
    c(
      "Unknown", "State Road", "Road", "Street", "Path", "Track", "Cycleway",
      "Footway", "Steps", "Ferry", "Construction"
    ),
    c(
      "mtb:scale=6", "mtb:scale=5", "mtb:scale=4", "mtb:scale=3", "mtb:scale=2",
      "mtb:scale=1", "mtb:scale=0", "No Tag", "sac_scale=hiking",
      "sac_scale=mountain_hiking", "sac_scale=demanding_mountain_hiking",
      "sac_scale=alpine_hiking", "sac_scale=demanding_alpine_hiking",
      "sac_scale=difficult_alpine_hiking"
    ),
    c(
      "None", "No", "Customers", "Destination",
      "Delivery", "Private", "Permissive"
    )
  ),
  profile = c(NA, NA, NA, NA, "profile", "NA"),
  ordinal = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE),
  base2 = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
)


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
#' @param res Response list from `call_ors_directions`
#' @param abort_on_error Whether to abort when an error code is returned
#' @param warn_on_warning Whether to warn when a warning code is returned
#' @noRd
handle_ors_conditions <- function(res, abort_on_error = FALSE, warn_on_warning = FALSE) {
  if (!is_ors_error(res)) {
    msg <- res$error
    code <- NULL
    
    if (!is.character(res$error)) {
      msg <- msg$message
      code <- res$error$code
    }
    
    if (is.null(msg) && !is.null(code)) {
      message <- fill_empty_error_message(code)
    }
    
    error <- paste0(
      ifelse(!is.null(code), "Error code ", ""), code, ": ", msg
    )
    if (abort_on_error) {
      cli::cli_abort(c("ORS encountered the following exception:", error))
    } else {
      structure(error, error = TRUE)
    }
  } else {
    warnings <- get_ors_warnings(res)
    message <- warnings$message
    code <- warnings$code

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


store_condition <- function(what, when, which) {
  has_error <- attr(what, "error")
  
  if (is.null(has_error)) {
    what <- NA
  }
  
  if (isFALSE(has_error)) {
    what <- unlist(what)
  }
  
  ors_cache$routing_conditions[[when]][which] <- what
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
  steps <- get_ors_steps(res)
  waypoints <- steps$way_points
  wp_distances <- steps$distance
  wp_durations <- steps$duration
  exp_distances <- expand_by_waypoint(wp_distances, waypoints)
  percentages <- units::drop_units(distances / expanded_wp_distances)
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
  last_waypoint <- get_ors_waypoints_range(res)[[2L]]
  matrix <- get_ors_extras(res, which = info_type)
  browser()
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
