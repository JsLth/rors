#' Extract points of interest from OpenStreetMap
#' 
#' @description Makes Overpass requests to extract points of interest within
#' a specified area or list of areas.
#'
#' @param source \code{[various]}
#' 
#' Either (1) an \code{sf} object containing coordinates, (2) a
#' character string of a place or region, (3) a spatial polygon as an sf or sfc
#' object or (4) any bbox format supported by \code{\link[osmdata]{opq}}. The
#' source argument is passed to osmdata to derive a bounding box for the
#' Overpass query.
#' @param ... Can be used to pass key-value combinations of OSM map features.
#' Map features are passed as arguments: key = "value". For a list of features,
#' refer to the
#' \href{https://wiki.openstreetmap.org/wiki/Map_features}{documentation}.
#' @param radius \code{[numeric]}
#' 
#' If \code{source} is an \code{sf} data.frame containing point geometries,
#' specifies the buffer radius within which points of interest should be
#' searched.
#' @param timeout \code{[integer]}
#' 
#' Timeout limit for the Overpass query. See \code{\link[osmdata]{opq}}. For
#' larger queries, make sure to pass a high enough timeout.
#' @param do_bind \code{[logical]}
#' 
#' If \code{TRUE} and \code{source} is an \code{sf} data.frame containing point
#' geometries, binds results to a single data.frame and returns only distinct
#' points of interest. Otherwise, returns a list of data.frames.
#' @returns
#' If \code{do_bind = FALSE} and \code{source} is an \code{sf} data.frame
#' containing point, geometries, returns a list of \code{sf} data.frames that
#' can be used for \code{\link[ORSRouting]{ors_shortest_distances}}. If
#' \code{do_bind = TRUE} or if \code{source} is a polygon, a place name or a
#' bounding box, returns a single \code{sf} data.frame with distinct points of
#' interest.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' sample_a <- ors_sample(20)
#' 
#' pois_source <- get_osm_pois(sample_a, amenity = "hospital", radius = 5000, crs = 4326)
#'
#' set.seed(123)
#' test_sf <- ors_sample(20, as_sf = TRUE)
#' test_poly <- sf::st_convex_hull(sf::st_union(test_sf))
#' get_osm_pois(test_poly, amenity = "post_box", building = "residential", trim = FALSE)
#'
#' test_bbox <- as.numeric(sf::st_bbox(test_poly))
#' get_osm_pois(test_bbox, amenity = "marketplace", as_sf = TRUE)
#'
#' test_place <- "Cologne"
#' get_osm_pois(test_place, landuse = "allotments", timeout = 200, as_sf = TRUE)
#' }

get_osm_pois <- function(source, ..., radius = 5000L, timeout = NULL, do_bind = FALSE) {
  if (!missing(...)) {
    feat <- list(...)
    feat <- paste(
      paste0('"', names(feat), '"'),
      paste0('"', feat, '"'),
      sep = "="
    )
  } else {
    cli::cli_abort(c(
      "No map features specified.",
      "i" = "To provide map features, pass key-value combinations like so: {.code amenity = \"hospital\"}"
    ))
  }

  if (is.sf(source)) {
    if (all(sf::st_is(source, c("POINT", "MULTIPOINT")))) {
      if (sf::st_crs(source)$epsg != 4326L) source <- sf::st_transform(source, 4326L)
      buffers <- sf::st_buffer(source, dist = radius)
      bbox <- lapply(seq_len(nrow(buffers)), function(i) {
        sf::st_bbox(buffers[i, ])
      })
      bbox <- do.call(rbind.data.frame, bbox)
    } else if (all(sf::st_is(source, c("POLYGON", "MULTIPOLYGON")))) {
      source <- sf::st_as_sf(source)
      buffers <- source
      bbox <- sf::st_bbox(source)
    } else {
      geom_type <- unique(sf::st_geometry_type(data))
      cli::cli_abort("Cannot derive boundaries from geometry type{?s} {.val geom_type}")
    }
  } else if (is.character(source) || is.numeric(source)) {
    bbox <- source
    buffers <- NULL
  } else {
    cli::cli_abort("Input of class {.cls class(source)} is not a supported format.")
  }

  timeout <- min(timeout, 25L)

  query_osm <- function(bbox, buffer) {
    if (is.character(bbox)) buffer <- osmdata::getbb(bbox, format_out = "sf_polygon")
    q <- osmdata::opq(bbox = unlist(bbox, use.names = FALSE), timeout = timeout)
    q <- osmdata::add_osm_features(q, features = feat)
    res <- osmdata::osmdata_sf(q)$osm_polygons
    
    if (nrow(res)) {
      sf::st_geometry(res) <- sf::st_centroid(sf::st_geometry(res))
      
      if (!is.null(buffers)) {
        within <- sf::st_within(res, buffer, sparse = FALSE)
        poly <- res[within, ]
      }
    }
    
    res
  }

  if (is.sf(source) && nrow(source) > 1) {
    pois <- lapply(seq_len(nrow(source)), function(i) query_osm(bbox[i, ], buffers[i, ]))
    if (isTRUE(do_bind)) {
      pois <- rbind_list(pois)
      pois <- pois[!duplicated(pois), ]
    } else {
      if (length(pois) == 1L) pois <- pois[[1L]]
    }
  } else {
    pois <- query_osm(bbox, buffers)
  }

  pois
}


#' Find the nearest points of interest from a local dataset
#' @description Returns points of interest in the proximity of the source
#' dataset. Unlike \code{\link{get_osm_pois}}, this function requires a local
#' dataset of points of interest.
#'
#' @param source Source dataset that represents point coordinates that are to
#' be routed from. The source dataset should be passed as a dataframe or plain
#' nested list with each row representing a x/y or lon/lat coordinate pair.
#' @param pois Dataset that represents a list of points of interest to be
#' routed to for each coordinate pair in the source dataset. The POI dataset
#' can either be passed as a dataframe or plain nested list with each row
#' representing a x/y or lon/lat coordinate pair, or, alternatively, as a
#' simple features object. If radius is not `NULL`, the POI dataset will be
#' converted to an sf object if it is not already.
#' @param number_of_points Integer scalar. Number of points to be returned.
#' @param radius Numeric scalar. Specifies the buffer size to select points of
#' interest by distance
#' @param crs Any object that is recognized by \code{\link[sf]{st_crs}}.
#' Specifies the coordinate notation of the source and pois dataset, if it they
#' are provided as simple coordinates.
#' @returns List of dataframes with each dataframe containing all points of
#' interest in a given type of proximity to the respective source point.
#'
#' @export
#'
#' @details The proximity can either be defined by the number of points to be selected, by a
#' distance buffer or by both. If both measures are defined, the function will select points
#' of interest within a certain radius first and will then select a given number of points
#' within that radius. The proximity type can be controlled by either passing or not passing
#' a value to `number_of_points` and `radius`.
#' @examples
#' \dontrun{
#' sample <- ors_sample(20)
#' pois <- get_osm_pois("Cologne", amenity = "hospital")
#' 
#' by_points <- get_nearest_pois(sample, pois, number_of_points = 5)
#' by_buffer <- get_nearest_pois(sample, pois, radius = 5000)
#' by_both <- get_nearest_pois(sample_pois, number_of_points = 5, radius = 5000)
#' }

get_nearest_pois <- function(source,
                             pois,
                             number_of_points = NULL,
                             radius = NULL,
                             crs = NA) {
  if (is.numeric(number_of_points)) {
    if (is.null(radius)) {
      # A number of points, but no radius is given -> n.nearest.pois
      n.nearest.pois(source, pois, number_of_points)
    }

    else if (is.numeric(radius)) {
      # A number of points and a radius is given -> pois.within.radius and then
      # n.nearest.pois
      pois_within_radius <- pois.within.radius(source, pois, radius, crs)
      n_pois_within_radius <- lapply(seq_along(pois_within_radius), function(x) {
        p <- n.nearest.pois(source[x, ], pois_within_radius[[x]], number_of_points)
      })
      n_pois_within_radius
    }

    else {
      cli::cli_abort(c("Radius must be either numeric or {.var NULL}.",
                       "Got {.cls {class(radius)}} instead."))
    }
  }

  else if (is.null(number_of_points) && is.numeric(radius)) {
    # No number of points, but a radius is given -> pois.within.radius
    pois.within.radius(source, pois, radius, crs)
  }

  else {
    cli::cli_abort("Either a radius, number of points, or both must be defined.")
  }
}


n.nearest.pois <- function(source, poi_coordinates, n) {
  if(is.sf(poi_coordinates)) {
    poi_coordinates <- reformat_vectordata(poi_coordinates)
  }

  if (is.sf(source)) {
    source <- reformat_vectordata(source)
  }
  
  select_lowest_distance <- function(number_index, matrix_index, dist_mat, sorted_dist_mat) {
    if (number_index <= nrow(poi_coordinates)) {
      # Selects the POI dataset index with the lowest distance to the
      # respective source coordinate
      index <- which(
        dist_mat[[matrix_index]] == sorted_dist_mat[[matrix_index]][number_index]
      )
      # ... and returns the POIs with the selected index
      poi_coordinates[index, ]
    }
  }

  create_distance_matrix <- function(spi) {
    comb_data <- rbind(source[spi, ], poi_coordinates)
    dist_matrix <- stats::dist(comb_data, method = "euclidean")
    cross_dist <- matrix(dist_matrix[seq_len(nrow(poi_coordinates))], nrow = 1L)
    cross_dist
  }
  
  # Creates unsorted distance matrices that keep the index order of the POI dataset so that
  # the shortest distance can easily be matched with their true index
  distance_matrices <- lapply(seq_len(nrow(source)), create_distance_matrix)
  
  # Creates sorted distance matrices to easily get the shortest distance by index number
  sorted_distance_matrices <- lapply(seq_len(nrow(source)), function(x) {
    as.matrix(sort(create_distance_matrix(x)))
  })
  
  # Creates a dataframe to nest the pmap loop so that it loops over n with each
  # iteration of the source dataset
  nested_iterator <- expand.grid(seq_len(n), seq_len(nrow(source)))
  output <- lapply(seq_len(nrow(nested_iterator)), function(i) {
    select_lowest_distance(
      nested_iterator[i, 1L],
      nested_iterator[i, 2L],
      distance_matrices,
      sorted_distance_matrices
    )
  })
  
  not_assigned <- sapply(output, is.null)
  nal <- length(output[not_assigned])
  if (nal) {
    i <- get("i", envir = parent.frame(2L))
    if (!n - nal) {
      cli::cli_warn("Point {.val {i}} could not be assigned any point{?s} of interest.")
    } else {
      cli::cli_warn("Point {.val {i}} could only be assigned {.val {n - nal}} point{?s} of interest.")
    }

  }
  output[not_assigned] <- NULL
  if (length(output)) {
    output <- as.data.frame(do.call(rbind, output), row.names = seq_along(output))
    output <- split(output, nested_iterator[seq_len(nrow(output)), 2L])
    output <- unname(output)
    output <- lapply(output, function(r) { row.names(r) <- NULL; r })
    
    if (length(output) == 1L) output <- output[[1]]
  } else {
    output <- data.frame()
  }
  
  output
}


pois.within.radius <- function(source, pois, radius, crs = NULL) {
  if (!is.sf(pois)) {
    if (!is.na(crs)) {
      # If the POIs are passed as coordinates and the coordinate notation is
      # known, convert coordinates to features.
      pois <-  sf::st_as_sf(as.data.frame(pois), coords = c(1,2), crs = crs)
    } else {
      cli::cli_abort("CRS must be specified for non-sf objects.")
    }
  }

  if (!is.sf(source)) {
    if (!is.na(crs)) {
      points <- sf::st_transform(
        sf::st_as_sf(source, coords = c(1L, 2L), crs = crs),
        sf::st_crs(pois)
      )
    } else {
      cli::cli_abort("CRS must be specified for non-sf objects.")
    }
  }

  crs <- sf::st_crs(pois) # Save old crs
  pois <- lonlat_to_utm(pois) # ... then reproject

  # Create distance radius as buffer polygon
  buffers <- buffers_from_points(points, radius)
  buffers <- sf::st_transform(buffers, sf::st_crs(pois))
  
  select.pois.within <- function (source_index) {
    # Selects all POIs within the respective distance buffer
    pois_within <- sf::st_within(pois, buffers[source_index, ], sparse = FALSE)
    sf::st_coordinates(sf::st_transform(sf::st_geometry(pois)[pois_within], crs))
  }
  
  selected_pois <- lapply(seq_len(nrow(source)), select.pois.within)
  
  selected_pois
}
