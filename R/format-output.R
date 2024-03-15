apply_shortest_routes <- function(index,
                                  src,
                                  dst,
                                  iter,
                                  units,
                                  geometry,
                                  instance,
                                  type,
                                  ...) {
  routes <- suppressWarnings(
    ors_pairwise(
      src = src[iter[index, "point_number"], ],
      dst = if (is.data.frame(dst)) {
        dst
      } else if (is.list(dst)) {
        dst[[iter[index, "point_number"]]]
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



route_to_df <- function(res,
                        level = "waypoint",
                        elevation = TRUE,
                        navigation = FALSE,
                        elev_as_z = FALSE,
                        params = list()) {
  alt <- get_ors_alternatives(res)

  rlist <- lapply(seq_len(alt), function(i) {
    # get waypoints from steps for each segment
    route <- get_ors_waypoints(res, i)

    if (level == "segment") {
      route[c("type", "instruction", "exit_number")] <- NULL
    }

    # combine waypoints with geometry
    route <- sf::st_sf(
      route,
      geometry = ors_multiple_linestrings(res, alt = i)
    )

    # extract z variable
    if (elevation && !elev_as_z) {
      coords <- sf::st_coordinates(route)
      route$elevation <- coords[!duplicated(coords[, "L1"]), "Z"]
      units(route$elevation) <- "m"
      route <- sf::st_zm(route)
    }

    # derive waypoint metrics from geometry
    if (level == "waypoint") {
      # get distances by measuring geometry length
      distances <- calculate_distances(route)
      # then derive durations by computing percentage of measured distances from
      # aggregated distances
      durations <- calculate_durations(route, distances)
      route[c("distance", "duration")] <- data.frame(distances, durations)
    }
    route$avgspeed <- calculate_avgspeed(route$distance, route$duration)

    # extract attributes
    params$attributes <- c(
      if (elevation) c("ascent", "descent"),
      if (level == "segment") c("distance", "duration"),
      params$attributes
    )
    attrib <- get_ors_attributes(res, which = params$attributes, alt = i)

    # extract and format extra info
    extra_info <- lapply(
      params$extra_info,
      function(x) format_extra_info(res, x, i)
    )
    extra_info <- do.call(cbind.data.frame, extra_info)
    names(extra_info) <- params$extra_info
    if (ncol(extra_info)) {
      route <- cbind(route, extra_info)
    }

    # aggregate in case level is not "waypoint"
    if (level != "waypoint") {
      if (level == "segment") {
        route[names(attrib)] <- NA
      }

      route <- by(
        route[3L:ncol(route)],
        INDICES = route[[level]],
        FUN = aggregate_route,
        level = level,
        attrib = attrib
      )
      route <- do.call(rbind.data.frame, route)
    }

    # reorder columns
    route <- reorder_route_columns(route)

    # make sure units are set properly
    units(route$distance) <- res$metadata$query$units
    units(route$duration) <- "s"
    units(route$avgspeed) <- "km/h"

    sf::st_as_sf(data_frame(route))
  })

  if (length(rlist) > 1) {
    names(rlist) <- c(
      "recommended",
      paste("alternative", seq_len(alt - 1L), sep = "_")
    )
  } else {
    rlist <- rlist[[1]]
  }

  rlist
}


aggregate_route <- function(route_section, level, attrib) {
  vals <- lapply(names(route_section), function(x) {
    if (inherits(route_section[[x]], "sfc")) {
      return(sf::st_combine(route_section[x]))
    }
    if (x %in% "elevation") {
      return(mean(route_section[[x]]))
    }
    if (level == "segment" && x %in% names(attrib)) {
      i <- get("i", envir = parent.frame(4))
      return(as.numeric(attrib[[x]][i]))
    }
    o <- unique(route_section[[x]])
    if (length(o) > 1) {
      return(count(route_section[[x]])[1, 1])
    }
    o
  })
  stats::setNames(do.call(cbind.data.frame, vals), names(route_section))
}



#' Replace response values with more informative ones
#' @param values Object from the response list
#' @param info_type Type of information to be replaced
#' @noRd
fill_extra_info <- function(codes, info_type, profile) {
  fill_table <- fill_table()
  tab <- fill_table[fill_table$name %in% info_type, ]

  # convert 0/1 to logical
  if (info_type %in% "tollways") {
    codes <- as.logical(codes)
  }

  # convert characters to (un)ordered factors
  if (nrow(tab)) {
    profile <- eval(str2lang(tab$profile))
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


fill_table <- function() {
  tab <- list(
    name = c(
      "steepness", "surface", "waycategory", "waytypes", "traildifficulty",
      "roadaccessrestrictions", "countryinfo"
    ),
    levels = list(
      -5:5, 0:18, c(0, 1, 2, 4, 8, 16, 32, 64, 128), 0:10, -7:6,
      c(0, 1, 2, 4, 8, 16, 32), 1:236
    ),
    labels = list(
      c(
        ">16% decline", "12-15% decline", "7-11% decline", "4-6% decline",
        "1-3% decline", "0% incline", "1-3% incline", "4-6% incline", "7-11% incline",
        "12-15% incline", ">16% incline"
      ),
      c(
        "Unknown", "Paved", "Unpaved", "Asphalt", "Concrete", "Cobblestone", "Metal",
        "Wood", "Compacted Gravel", "Fine Gravel", "Gravel", "Dirt", "Ground", "Ice",
        "Paving Stones", "Sand", "Woodchips", "Grass", "Grass Paver"
      ),
      c(
        "No category", "Highway", "Steps", "Unpaved Road", "Ferry", "Track", "Tunnel",
        "Paved Road", "Ford"
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
      c("None", "No", "Customers", "Destination", "Delivery", "Private", "Permissive"),
      country_info
    ),
    profile = rep(c(NA, "profile", NA), c(4L, 1L, 2L)),
    ordinal = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    base2 = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
  )

  class(tab) <- "data.frame"
  attr(tab, "row.names") <- 1:7
  tab
}


calculate_distances <- function(waypoints) {
  round(sf::st_length(waypoints), 2)
}


calculate_avgspeed <- function(distances, durations) {
  speeds <- distances / durations
  units(speeds) <- "m/s"
  round(units::set_units(speeds, "km/h"), 2L)
}


calculate_durations <- function(waypoints, distances) {
  distances <- units::drop_units(distances)
  wp_distances <- waypoints$distance
  wp_durations <- waypoints$duration
  wp <- as.numeric(row.names(waypoints))
  percentages <- distances / wp_distances
  durations <- wp_durations * percentages
  units(durations) <- "s"
  round(durations, 2L)
}


get_waypoint_index <- function(from, to, waypoints, by_waypoint) {
  if (isFALSE(by_waypoint)) {
    from_index <- match(from + 1L, waypoints[[1L]])
    to_index <- match(to + 1L, waypoints[[2L]])
    seq(from_index, to_index)
  }
}


format_extra_info <- function(res, info_type, alt = 1L) {
  if (identical(info_type, "waytype")) info_type <- "waytypes"
  last_waypoint <- utils::tail(get_ors_waypoints_range(res, alt = alt), 1L)
  matrix <- get_ors_extras(res, which = info_type, alt = alt)

  if (length(matrix)) {
    start <- matrix[, 1L]
    end <- matrix[, 2L]

    iterator <- data.frame(
      V1 = seq(1L, last_waypoint),
      V2 = seq(1L, last_waypoint) + 1L
    )

    indices <- Map(
      FUN = get_waypoint_index,
      start,
      end,
      MoreArgs = list(waypoints = iterator, by_waypoint = FALSE)
    )

    values <- lapply(
      seq(1, length(indices)),
      function(seg) rep(matrix[seg, 3L], length(indices[[seg]]))
    )
    values <- unlist(values)

    profile <- res$metadata$query$profile
    fill_extra_info(values, info_type, profile)
  } else {
    rep(NA, last_waypoint)
  }
}


reorder_route_columns <- function(waypoints) {
  order_columns <- c(
    "name", "distance", "duration", "avgspeed", "elevation", "type",
    "instruction", "exit_number", "steepness", "suitability", "surface",
    "waycategory",  "waytype", "traildifficulty", "green", "noise",
    "detourfactor", "percentage", "geometry"
  )
  order_columns <- intersect(unique(order_columns), names(waypoints))
  waypoints[order_columns]
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
