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
#' @inheritParams ors_pairwise
#' @returns Returns \code{pois} with an added \code{.groups} column that links
#' each row to a row in the \code{src} dataset.
#'
#' @examples
#' \dontrun{
#' sample <- ors_sample(20)
#' pois <- get_osm_pois("Cologne", amenity = "hospital")
#'
#' by_points <- get_closest_pois(sample, pois, n = 5)
#' by_buffer <- get_closest_pois(sample, pois, radius = 5000)
#' by_both <- get_closest_pois(sample_pois, n = 5, radius = 5000)}
#' @export
get_closest_pois <- function(src,
                             pois,
                             n = NULL,
                             radius = NULL) {
  src <- prepare_input(src, to_coords = FALSE)
  pois <- prepare_input(pois, to_coords = FALSE)

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
      n_nearest_pois(src, pois, n)
    } else if (is.numeric(radius)) {
      # A number of points and a radius is given -> pois_within_radius and then
      # n_nearest_pois
      within_radius <- pois_within_radius(src, pois, radius)
      n_within_radius <- lapply(within_radius$.group, function(i) {
        n_nearest_pois(
          src[i, ],
          within_radius[which(within_radius$.group == i), ],
          n = n,
          group_index = i
        )
      })
      as_data_frame(do.call(rbind.data.frame, n_within_radius))
    } else {
      cli::cli_abort(c(
        "Radius must be either numeric or {.var NULL}.",
        "Got {.cls {class(radius)}} instead."
      ))
    }
  } else if (is.null(n) && is.numeric(radius)) {
    # No number of points, but a radius is given -> pois_within_radius
    pois_within_radius(src, pois, radius)
  } else {
    cli::cli_abort("Either a radius, number of points, or both must be provided.")
  }
}


#' Returns the n closest points of interest around each point in source
#'
#' @noRd
n_nearest_pois <- function(src, pois, n, group_index = NULL) {
  # Remove former groupings
  pois$.group <- NULL

  select_lowest_distance <- function(ni, mi, dmat, smat) {
    if (ni <= nrow(pois)) {
      index <- which(dmat[mi, ] %in% smat[mi, ni])
      sel <- pois[index, ]
      cbind(
        data_frame(.group = if (is.null(group_index)) mi else group_index),
        sel
      )
    } else {
      data_frame()
    }
  }

  # Create a distance matrix and sort it row-wise by distance
  dist_matrix <- unclass(sf::st_distance(src, pois))
  sorted_matrix <- t(apply(dist_matrix, MARGIN = 1, sort.int, method = "quick"))

  # For each source point, select the n pois with the lowest distance
  iter <- expand.grid(n = seq_len(n), m = seq_len(nrow(src)))
  output <- mapply(
    FUN = select_lowest_distance,
    iter[["n"]],
    iter[["m"]],
    MoreArgs = list(dmat = dist_matrix, smat = sorted_matrix),
    SIMPLIFY = FALSE
  )

  sf::st_as_sf(as_data_frame(do.call(rbind.data.frame, output)))
}


#' Returns points of interest that lie within a specified radius around each
#' point in source
#'
#' @noRd
pois_within_radius <- function(src, pois, radius) {
  buffers <- sf::st_buffer(src, radius)
  within <- sf::st_within(pois, buffers, sparse = FALSE)
  sel <- lapply(seq_len(ncol(within)), \(i) {
    cbind(
      data_frame(.group = rep.int(i, sum(within[, i]))),
      pois[within[, i], ]
    )
  })
  sel <- sf::st_as_sf(as_data_frame(do.call(rbind.data.frame, sel)))
  sel
}
