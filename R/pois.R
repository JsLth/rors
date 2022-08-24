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
#' can be used for \code{\link{ors_shortest_distances}}. If
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
#' test_sf <- ors_sample(20)
#' test_poly <- sf::st_convex_hull(sf::st_union(test_sf))
#' get_osm_pois(test_poly, amenity = "post_box", building = "residential", trim = FALSE)
#'
#' test_bbox <- as.numeric(sf::st_bbox(test_poly))
#' get_osm_pois(test_bbox, amenity = "marketplace")
#'
#' test_place <- "Cologne"
#' get_osm_pois(test_place, landuse = "allotments", timeout = 200)
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

  if (is_sf(source)) {
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
      geom_type <- unique(sf::st_geometry_type(source))
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
    res <- osmdata::osmdata_sf(q)

    if (nrow(res$osm_polygons)) {
      sf::st_geometry(res$osm_polygons) <- sf::st_centroid(sf::st_geometry(res$osm_polygons))

      if (!is.null(buffers)) {
        within <- sf::st_within(res$osm_polygons, buffer, sparse = FALSE)
        poly <- res$osm_polygons[within, ]
      }
    }

    res <- rbind_list(res[names(res) %in% c("osm_points", "osm_polygons")])

    res
  }

  if (is_sf(source) && nrow(source) > 1) {
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

  sf::st_as_sf(tibble::as_tibble(pois))
}


#' Group a POI dataset
#' @description Groups a dataset containing points of interest based on their
#' proximity to a source dataset. Proximity can be defined through physical
#' distance (argument \code{n}) and/or a distance buffer (\code{radius}).
#'
#' @param pois \code{[sf]}
#'
#' Dataset that represents a list of points of interest to be routed to for each
#' row in the source dataset
#' @param n \code{[numeric]}
#'
#' Maximum number of points of interest around each point in the source dataset
#' that shall be returned. The actual number might be lower depending on the
#' rows in the \code{pois} dataset and the remaining number of points if
#' \code{radius} is not \code{NULL}. If \code{NULL}, \code{radius} must be
#' provided.
#' @param radius \code{[numeric]}
#'
#' Maximum distance of a point of interest around each point in the source
#' dataset. All returned points of interest lie within this distance to the
#' source points. If \code{NULL}, \code{n} must be provided.
#' @inheritParams ors_distances
#' @returns Returns \code{pois} with an added \code{.groups} column that links
#' each row to a row in the \code{source} dataset.
#'
#' @examples
#' \dontrun{
#' sample <- ors_sample(20)
#' pois <- get_osm_pois("Cologne", amenity = "hospital")
#'
#' by_points <- get_closest_pois(sample, pois, n = 5)
#' by_buffer <- get_closest_pois(sample, pois, radius = 5000)
#' by_both <- get_closest_pois(sample_pois, n = 5, radius = 5000)
#' }
#'
#' @export
get_closest_pois <- function(source,
                             pois,
                             n = NULL,
                             radius = NULL) {
  source <- format_input_data(source)
  pois <- format_input_data(pois)

  if (!is.null(n) && n > nrow(pois)) {
    cli::cli_warn(c(
      "!" = "Argument {.var n} is greater than the number of rows in {.var pois}",
      "i" = "{.var n} changed to {.code nrow(pois)}."
    ))
    n <- nrow(pois)
  }

  if (is.numeric(n)) {
    if (is.null(radius)) {
      # A number of points, but no radius is given -> n.nearest.pois
      n_nearest_pois(source, pois, n)
    } else if (is.numeric(radius)) {
      # A number of points and a radius is given -> pois_within_radius and then
      # n_nearest_pois
      within_radius <- pois_within_radius(source, pois, radius)
      n_within_radius <- lapply(within_radius$.group, function(i) {
        n_nearest_pois(
          source[i, ],
          within_radius[which(within_radius$.group == i), ],
          n = n,
          group_index = i
        )
      })
      tibble::as_tibble(do.call(rbind.data.frame, n_within_radius))
    } else {
      cli::cli_abort(c(
        "Radius must be either numeric or {.var NULL}.",
        "Got {.cls {class(radius)}} instead."
      ))
    }
  } else if (is.null(n) && is.numeric(radius)) {
    # No number of points, but a radius is given -> pois_within_radius
    pois_within_radius(source, pois, radius)
  } else {
    cli::cli_abort("Either a radius, number of points, or both must be provided.")
  }
}


#' Returns the n closest points of interest around each point in source
#'
#' @noRd
n_nearest_pois <- function(source, pois, n, group_index = NULL) {
  # Remove former groupings
  pois$.group <- NULL

  select_lowest_distance <- function(ni, mi, dmat, smat) {
    if (ni <= nrow(pois)) {
      index <- which(dmat[mi, ] %in% smat[mi, ni])
      sel <- pois[index, ]
      cbind(
        tibble::tibble(.group = if (is.null(group_index)) mi else group_index),
        sel
      )
    } else {
      tibble::tibble()
    }
  }

  # Create a distance matrix and sort it row-wise by distance
  dist_matrix <- unclass(sf::st_distance(source, pois))
  sorted_matrix <- t(apply(dist_matrix, MARGIN = 1, sort.int, method = "quick"))

  # For each source point, select the n pois with the lowest distance
  iter <- expand.grid(n = seq_len(n), m = seq_len(nrow(source)))
  output <- mapply(
    FUN = select_lowest_distance,
    iter[["n"]],
    iter[["m"]],
    MoreArgs = list(dmat = dist_matrix, smat = sorted_matrix),
    SIMPLIFY = FALSE
  )

  sf::st_as_sf(tibble::as_tibble(do.call(rbind.data.frame, output)))
}


#' Returns points of interest that lie within a specified radius around each
#' point in source
#'
#' @noRd
pois_within_radius <- function(source, pois, radius) {
  buffers <- sf::st_buffer(source, radius)
  within <- sf::st_within(pois, buffers, sparse = FALSE)
  sel <- lapply(seq_len(ncol(within)), \(i) {
    cbind(
      tibble::tibble(.group = rep.int(i, sum(within[, i]))),
      pois[within[, i], ]
    )
  })
  sel <- sf::st_as_sf(tibble::as_tibble(do.call(rbind.data.frame, sel)))
  sel
}
