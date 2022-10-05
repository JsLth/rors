get_ors_geometry <- function(res, as_coords = FALSE) {
  if (missing(res)) {
    return(sf::st_sfc(sf::st_linestring()))
  }
  
  if (!is_ors_geojson(res)) {
    return(NULL)
  }
  
  res <- get_ors_features(res)
  geom <- res$geometry$coordinates[[1]]
  
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
    if (geometry) 
      summ <- sf::st_sf(summ, geometry = get_ors_geometry())
  } else if (is_ors_geojson(res)) {
    summ <- res$features$properties$summary
    summ <- sf::st_sf(summ, geometry = get_ors_geometry(res))
  } else {
    summ <- res$routes$summary
  }

  if (!ncol(summ)) {
    summ[c("distance", "duration")] <- 0
  }

  summ
}


get_ors_extras <- function(res, which = NULL) {
  if (is_ors_geojson(res)) {
    extras <- res$features$properties$extras
    if (!is.null(which)) {
      extras <- extras[[which]]$values[[1L]]
    }
  }
  extras
}


get_ors_attributes <- function(res, which) {
  segments <- res$features$properties$segments
  stats::setNames(lapply(which, \(x) segments[[x]]), which)
}


get_ors_waypoints_range <- function(res) {
  res$features$properties$way_points[[1L]]
}


get_ors_steps <- function(res, bind = TRUE) {
  if (is_ors_geojson(res)) {
    steps <- res$features$properties$segments[[1L]]$steps
    if (bind) {
      steps <- lapply(seq_along(steps), \(i) cbind(segment = i, steps[[i]]))
      rbind_list(steps)
    }
    steps
  }
}


get_ors_warnings <- function(res) {
  if (is_ors_error(res)) {
    return(NULL)
  }
  
  if (is_ors_geojson(res)) {
    res$features$properties$warnings[[1L]]
  } else {
    res$routes$warnings[[1L]]
  }
}


is_ors_geojson <- function(res) {
  res$metadata$query$format == "geojson"
}


is_ors_error <- function(res) {
  !is.null(res$error)
}


get_ors_features <- function(res) {
  if (is_ors_geojson(res)) {
    res$features
  } else {
    res$routes
  }
}