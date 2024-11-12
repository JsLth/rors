tidy_route <- function(res, ...) {
  alt <- get_ors_alternatives(res)
  routes <- lapply(seq_len(alt), tidy_alternative, res = res, ...)

  if (length(routes) > 1) {
    alt_names <- c("recommended", paste("alt", seq_len(alt - 1L)))
    names(routes) <- alt_names
    bind_rows(routes, .id = "alt")
  } else {
    routes[[1]]
  }
}


tidy_alternative <- function(alt,
                             res,
                             level = "waypoint",
                             elevation = TRUE,
                             navigation = FALSE,
                             elev_as_z = FALSE,
                             params = list()) {
  # get waypoints from steps for each segment
  route <- get_ors_waypoints(res, alt)

  # instructions are not meaningful at the highest aggregation level
  if (level == "segment" || !navigation) {
    route[c("type", "instruction", "exit_number")] <- NULL
  }

  # combine waypoints with geometry
  route <- sf::st_sf(
    route,
    geometry = ors_multiple_linestrings(res, alt = alt)
  )

  # extract z variable and remove it from geometry
  if (elevation && !elev_as_z) {
    coords <- sf::st_coordinates(route)
    route$elevation <- coords[!duplicated(coords[, "L1"]), "Z"]
    route <- sf::st_zm(route)
  }

  # derive waypoint metrics from geometry
  if (level == "waypoint") {
    # distances and durations are included in the response but only at
    # a segment and step level. to derive these values at a waypoint level,
    # measure the length of the linestring geometry
    route$distance <- estimate_distances(route)
    # ... and then estimate durations by calculating the percentage of
    # measured distances from the aggregated distances
    # then derive durations by computing percentage of measured distances from
    # aggregated distances
    route$duration <- estimate_durations(route, route$distance)
  }
  route$avgspeed <- estimate_avgspeed(route$distance, route$duration)

  # extract attributes
  attribs <- c(
    if (elevation) c("ascent", "descent"),
    if (level == "segment") c("distance", "duration"),
    params$attributes
  )
  attrib <- get_ors_attributes(res, which = attribs, alt = alt)

  # extract and format extra info
  extra_info <- lapply(params$extra_info, format_extra_info, res, alt)
  extra_info <- do.call(cbind.data.frame, extra_info)
  names(extra_info) <- params$extra_info
  if (ncol(extra_info)) {
    route <- cbind(route, extra_info)
  }

  # aggregate in case level is not "waypoint"
  if (level != "waypoint") {
    sidx <- 2

    # some columns are already covered by attributes at the segment level
    if (level == "segment") {
      route[names(attrib)] <- NA
      sidx <- 3
    }

    route <- by(
      route[sidx:ncol(route)],
      INDICES = route[[level]],
      FUN = aggregate_route,
      level = level,
      attrib = attrib
    )
    route <- do.call(rbind.data.frame, route)
  }

  # reorder columns
  route <- reorder_route_columns(route)

  sf::st_as_sf(data_frame(route))
}


aggregate_route <- function(route, level, attrib) {
  vals <- lapply(seq_along(route), function(i) {
    col <- names(route)[i]
    val <- route[[i]]

    if (inherits(val, "sfc")) {
      return(sf::st_combine(val))
    }

    # distances and durations are constant on a step level and native
    # on a segment level -> only take mean of elevation
    if (col %in% "elevation") {
      return(mean(val))
    }

    if (level == "segment" && col %in% names(attrib)) {
      return(as.numeric(attrib[[col]][i]))
    }

    uval <- unique(val)
    if (length(uval) == 1) {
      return(uval)
    }

    Mode(val)
  })
  vals <- do.call(cbind.data.frame, vals)
  names(vals) <- names(route)
  vals
}


estimate_distances <- function(waypoints) {
  round(sf::st_length(waypoints), 2)
}


estimate_avgspeed <- function(distances, durations) {
  speeds <- distances / durations
  round(speeds * 3.6, 2L)
}


estimate_durations <- function(waypoints, distances) {
  wp_distances <- waypoints$distance
  wp_durations <- waypoints$duration
  wp <- as.numeric(row.names(waypoints))
  percentages <- distances / wp_distances
  durations <- wp_durations * percentages
  round(durations, 2L)
}


#' Retrieves extra info from the response and performs some sort of linear
#' referencing to retrieve an extra info value for each waypoint. Also assigns
#' labels to each code. If no extra info is available, returns NA.
#' @param res response list
#' @param info_type the type of extra info to format, e.g. steepness
#' @param alt index of route alternative; usually, this is just 1.
#' @noRd
format_extra_info <- function(info_type, res, alt = 1) {
  if (identical(info_type, "waytype")) info_type <- "waytypes"
  last_waypoint <- last(get_ors_waypoints_range(res, alt = alt))
  extras <- get_ors_extras(res, which = info_type, alt = alt)

  if (length(extras)) {
    # get a vector of the number of waypoints per step
    start <- extras[, 1L]
    n_waypoints <- diff(c(start, last_waypoint))

    # create a list where each element is a step containing waypoints
    # then replace all waypoints with their matching extra info codes
    values <- .mapply(rep, dots = list(extras[, 3], n_waypoints), NULL)
    values <- unlist(values)

    # replace info codes with human-readable labels
    fill_extra_info(values, info_type)
  } else {
    rep(NA, last_waypoint)
  }
}


#' Replace response values with more informative ones
#' @param values Object from the response list
#' @param info_type Type of information to be replaced
#' @noRd
fill_extra_info <- function(codes, info_type) {
  # convert 0/1 to logical
  if (info_type %in% "tollways") {
    codes <- as.logical(codes)
  }

  tab <- info_table(info_type)

  # convert characters to (un)ordered factors
  if (!is.null(tab)) {
    ordinal <- info_type %in% c("steepness", "traildifficulty")
    base2 <- info_type %in% c("waycategory", "roadaccessrestrictions")
    fct_fun <- ifelse(ordinal, ordered, factor)

    # replace base2 encoded values with their labels
    if (base2) {
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


reorder_route_columns <- function(waypoints) {
  order_cols <- c("name", "distance", "duration", "avgspeed")
  if ("elevation" %in% names(waypoints)) {
    order_cols <- c(order_cols, "elevation")
  }
  other_cols <- setdiff(names(waypoints), order_cols)
  other_cols <- setdiff(other_cols, "geometry")
  waypoints[c(order_cols, other_cols, "geometry")]
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
