#' Low-level response helpers
#' @description
#' Low-level utilities to extract information from a routing response. These
#' functions are base-bones helpers that do not perform any type checks and
#' are mainly designed for internal use.
#'
#' \itemize{
#'  \item{\code{get_ors_geometry()} extracts the geometries from a
#'  given response.}
#'  \item{\code{get_ors_summary()} extracts the distance and duration of the
#'  entire route.}
#'  \item{\code{get_ors_extras()} extracts a matrix of waypoints and corresponding
#'  extra info codes. See \code{\link{info_table}} to decode these values.}
#'  \item{\code{get_ors_attributes()} extracts attribute values for each segment
#'  of a route.}
#'  \item{\code{get_ors_waypoints_range()} extracts waypoint intervals for each
#'  route in the response.}
#'  \item{\code{get_ors_waypoints()} extracts a dataframe of waypoints and
#'  relevant metadata like distance, duration, and instructions.}
#'  \item{\code{get_ors_warnings()} extracts all warnings returned along with
#'  the response.}
#'  \item{\code{get_ors_features()} extracts all features or properties of
#'  a response.}
#'  \item{\code{get_ors_alternatives()} returns the number of alternative
#'  routes in a response.}
#' }
#'
#' @param res Parsed geojson response as returned by
#' \code{\link[=ors_inspect]{ors_inspect(..., as = "list")}}.
#' @param alt Alternative route to extract. Defaults to the recommended route.
#' Only relevant if alternative routes are computed.
#' @param as_coords Whether to return the geometry as an sf object or as
#' a dataframe.
#'
#' @returns \code{get_ors_geometry()} returns an object of class \code{sfc}
#' if \code{as_coords = FALSE}, otherwise a dataframe. If \code{res} is not
#' specified, returns an empty linestring. \code{get_ors_summary} returns an
#' object of class \code{sf} if \code{geometry = TRUE}, otherwise a dataframe.
#' \code{get_ors_extras} returns a 3-column matrix where the first column are
#' the start waypoints, the second column are the end waypoints and the third
#' column are the info codes. \code{get_ors_attributes} returns a dataframe
#' where each column is an attribute and each row is a segment.
#' \code{get_ors_waypoints_range} returns a numeric vector of waypoint indices.
#' \code{get_ors_waypoints} returns a dataframe where each row is a waypoint.
#' \code{get_ors_warnings} returns a 2-column dataframe containing the warning
#' code and message. \code{get_ors_features} returns a nested list.
#' \code{get_ors_alternatives} returns a single numeric value.
#'
#' @export
#'
#' @examples
#' \dontshow{httptest2::start_vignette("ors_inspect")}# retrieve an unformatted response
#' res <- ors_inspect(
#'   attributes = TRUE,
#'   extra_info = TRUE,
#'   navigation = TRUE,
#'   as = "list"
#' )
#'
#' # These low-level utilities can be used to more conveniently extract
#' # information from complex ORS response structures.
#' get_ors_geometry(res)
#' get_ors_summary(res)
#' get_ors_extras(res, which = "steepness")
#' get_ors_attributes(res, which = "ascent")
#' get_ors_waypoints_range()
#' get_ors_waypoints()
#' get_ors_warnings()
#' get_ors_alternatives()
get_ors_geometry <- function(res, alt = 1L, as_coords = FALSE) {
  if (missing(res)) {
    return(sf::st_sfc(sf::st_linestring(), crs = 4326))
  }

  if (!is_ors_geojson(res)) {
    return(NULL)
  }

  features <- get_ors_features(res, properties = FALSE)
  alt <- alt %NA% seq(1, length(features$geometry$coordinates))
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
  } else {
    colnames(geom) <- c("x", "y", if (ncol(geom) > 2) "z")
    geom <- as_data_frame(geom)
  }

  geom
}


#' @rdname get_ors_geometry
#' @param geometry Whether to return the geometry in addition to each route
#' summary.
#' @param ... Additional arguments passed to \code{get_ors_geometry}.
#' @export
get_ors_summary <- function(res, geometry = TRUE, ...) {
  if (is_ors_error(res)) {
    summ <- data.frame(distance = NA_real_, duration = NA_real_)
    if (geometry) {
      summ <- sf::st_sf(summ, geometry = get_ors_geometry())
    }
  } else {
    properties <- get_ors_features(res)
    summ <- properties$summary

    if (is_ors_geojson(res) && geometry) {
      summ <- sf::st_sf(summ, geometry = get_ors_geometry(res, ...))
    }

    if (!ncol(summ)) {
      summ[c("distance", "duration")] <- 0
    }
  }

  summ
}


#' @rdname get_ors_geometry
#' @param which Extra information (for \code{get_ors_extras}) or attribute
#' (for \code{get_ors_attributes}) to extract from the response. If \code{NULL},
#' extracts all extra information or attributes, respectively. Defaults to
#' \code{NULL}.
#' @export
get_ors_extras <- function(res, which = NULL, alt = 1L) {
  properties <- get_ors_features(res)
  extras <- properties$extras
  if (!is.null(which)) {
    extras <- extras[[which]]$values[[alt]]
  }
  extras
}


#' @rdname get_ors_geometry
#' @export
get_ors_attributes <- function(res, which = NULL, alt = 1L) {
  properties <- get_ors_features(res)
  segments <- properties$segments[[alt]]
  segments <- segments[!names(segments) %in% "steps"]

  if (!is.null(which)) {
    segments <- segments[which]
  }

  segments
}


#' @rdname get_ors_geometry
#' @export
get_ors_waypoints_range <- function(res, alt = 1L) {
  properties <- get_ors_features(res)
  properties$way_points[[alt]]
}


#' @rdname get_ors_geometry
#' @export
get_ors_waypoints <- function(res, alt = 1) {
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
    steps <- steps[rep(seq_len(nrow(steps)), reps),]

    steps$name <- gsub(pattern = "^-$", replacement = NA, steps$name)
    row.names(steps) <- NULL
    as_data_frame(steps)
  }
}


#' @rdname get_ors_geometry
#' @export
get_ors_warnings <- function(res) {
  if (is_ors_error(res)) {
    return(NULL)
  }

  if (is_ors_geojson(res)) {
    unbox(res$features$properties$warnings)
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


#' @rdname get_ors_geometry
#' @param properties If \code{TRUE}, extracts feature properties.
#' If \code{FALSE}, only extracts properties (the nesting level before
#' properties). Only relevant if \code{res} is a geojson.
#' @export
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


#' @rdname get_ors_geometry
#' @export
get_ors_alternatives <- function(res) {
  properties <- get_ors_features(res, properties = TRUE)
  length(properties$segments)
}
