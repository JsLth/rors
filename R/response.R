get_ors_geometry <- function(res, alt = 1L, as_coords = FALSE) {
  if (missing(res)) {
    return(sf::st_sfc(sf::st_linestring(), crs = 4326))
  }

  if (!is_ors_geojson(res)) {
    return(NULL)
  }

  features <- get_ors_features(res, properties = FALSE)

  if (is.na(alt)) {
    alt <- seq(1, length(features$geometry$coordinates))
  }

  geom <- features$geometry$coordinates[alt]

  if (length(geom) == 1) {
    geom <- geom[[1]]
  }

  if (!as_coords) {
    if (nrow(geom) > 1) {
      geom <- sf::st_linestring(geom)
    } else {
      geom <- sf::st_point(geom)
    }

    geom <- sf::st_sfc(geom, crs = 4326)
  }

  geom
}


get_ors_summary <- function(res, geometry = TRUE) {
  if (is_ors_error(res)) {
    summ <- data.frame(distance = NA, duration = NA)
    if (geometry) {
      summ <- sf::st_sf(
        summ,
        geometry = get_ors_geometry()
      )
    }
  } else {
    properties <- get_ors_features(res)
    summ <- properties$summary

    if (is_ors_geojson(res)) {
      summ <- sf::st_sf(summ, geometry = get_ors_geometry(res))
    }

    if (!ncol(summ)) {
      summ[c("distance", "duration")] <- 0
    }
  }

  summ
}


get_ors_extras <- function(res, which = NULL, alt = 1L) {
  properties <- get_ors_features(res)
  extras <- properties$extras
  if (!is.null(which)) {
    extras <- extras[[which]]$values[[alt]]
  }
  extras
}


get_ors_attributes <- function(res, which, alt = 1L) {
  properties <- get_ors_features(res)
  segments <- properties$segments[[alt]]
  elev_attrib <- c("ascent", "descent")
  if (all(elev_attrib %in% names(properties))) {
    segments <- c(segments, properties[elev_attrib])
  }
  stats::setNames(lapply(which, \(x) segments[[x]]), which)
}


get_ors_waypoints_range <- function(res, alt = 1L) {
  properties <- get_ors_features(res)
  properties$way_points[[alt]]
}


get_ors_waypoints <- function(res, alt = 1L) {
  if (is_ors_geojson(res)) {
    properties <- get_ors_features(res)

    # extract from response
    steps <- properties$segments[[alt]]$steps

    # construct a dataframe with segment indicator for each segment
    steps <- lapply(seq_along(steps), \(i) cbind(segment = i, steps[[i]]))

    # bind segment dataframes
    steps <- rbind_list(steps)
    steps <- cbind(step = as.numeric(row.names(steps)), steps)

    # find interval to expand steps to waypoints
    # (this removes all 0 distance waypoints)
    reps <- vapply(steps$way_points, \(x) x[2] - x[1], FUN.VALUE = numeric(1))
    steps$way_points <- NULL
    steps$distance <- as.numeric(steps$distance)
    steps$duration <- as.numeric(steps$duration)

    # expand dataframe
    steps <- steps[rep(1:nrow(steps), reps),]

    steps$name <- gsub(pattern = "^-$", replacement = NA, steps$name)
    row.names(steps) <- NULL
    as_data_frame(steps)
  }
}


get_ors_warnings <- function(res) {
  if (is_ors_error(res)) {
    return(NULL)
  }

  if (is_ors_geojson(res)) {
    warnings <- res$features$properties$warnings
  } else {
    res$routes$warnings[[1]]
  }
}


is_ors_geojson <- function(res) {
  if (!is.null(res$metadata$query$format)) {
    identical(res$metadata$query$format, "geojson")
  } else {
    identical(res$type, "FeatureCollection")
  }

}


is_ors_error <- function(res) {
  !is.null(res$error)
}


get_ors_features <- function(res, properties = TRUE) {
  if (is_ors_geojson(res)) {
    if (properties) {
      res$features$properties
    } else {
      res$features
    }
  } else {
    res$routes
  }
}


get_ors_alternatives <- function(res) {
  properties <- get_ors_features(res, properties = TRUE)
  length(properties$segments)
}
