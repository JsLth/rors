#' Group a dataset for routing
#' @description Groups a dataset containing points of interest based on their
#' proximity to a source dataset. Proximity can be defined through nearest
#' neighbors (argument \code{n}) and/or a distance buffer (\code{radius}).
#' The grouped output can be used as an input to
#' \code{\link{ors_shortest_distances}}.
#'
#' @param dst \code{[sf]}
#'
#' Dataset that represents a list of destination points to be routed to for each
#' row in the source dataset. For each point in \code{src}, a number of
#' nearest points in \code{dst} is selected.
#' @param n \code{[numeric]}
#'
#' Maximum number of points of interest around each point in the source dataset
#' that shall be returned. The actual number might be lower depending on the
#' rows in the \code{dst} dataset and the remaining number of points if
#' \code{radius} is not \code{NULL}. If \code{NULL}, \code{radius} must be
#' provided.
#' @param radius \code{[numeric]}
#'
#' Maximum distance of a point of interest around each point in the source
#' dataset. All returned points of interest lie within this distance to the
#' source points. If \code{NULL}, \code{n} must be provided.
#' @inheritParams ors_pairwise
#' @returns Returns an \code{sf} dataframe containing a \code{.group}
#' specifying the row in \code{src} for which the closest point in dst
#' is specified. The \code{geometry} column can contain duplicated geometries
#' because a point can be a closest point to multiple rows in \code{src}.
#'
#' @examples
#' \dontrun{
#' sample <- ors_sample(20)
#' dest <- ors_sample(20)
#'
#' # group by n nearest points
#' get_closest_dst(sample, dest, n = 5)
#'
#' # group by distance threshold
#' get_closest_dst(sample, dest, radius = 5000)
#'
#' # group by distance and then by nearest points
#' get_closest_dst(sample, dest, n = 5, radius = 5000)}
#' @export
group_by_proximity <- function(src,
                               dst,
                               n = NULL,
                               radius = NULL) {
  src <- prepare_input(src, to_coords = FALSE)
  dst <- prepare_input(dst, to_coords = FALSE)

  if (!is.null(n) && n > length(dst)) {
    msg <- c(
      "!" = "Argument {.var n} is greater than the number of items in {.var dst}",
      "i" = "{.var n} changed to {.code length(dst)}."
    )
    cli::cli_warn(msg, class = "ors_group_n_warn")
    n <- length(dst)
  }

  if (is.numeric(n)) {
    if (is.null(radius)) {
      # A number of points, but no radius is given -> n_nearest_points
      n_nearest_points(src, dst, n)
    } else if (is.numeric(radius)) {
      # A number of points and a radius is given -> points_within_radius and then
      # n_nearest_points
      within_radius <- points_within_radius(src, dst, radius)
      group <- unique(within_radius$.group)
      n_within_radius <- lapply(group, function(i) {
        within_radius <- within_radius[which(within_radius$.group == i), ]
        nwr <- group_by_proximity(src[i], within_radius, n = n)
        nwr$.group <- i
        nwr
      })
      as_data_frame(do.call(rbind.data.frame, n_within_radius))
    } else {
      msg <- c(
        "Radius must be either numeric or {.var NULL}.",
        "Got {.cls {class(radius)}} instead."
      )
      abort(msg, class = "group_invalid_radius_error")
    }
  } else if (is.null(n) && is.numeric(radius)) {
    # No number of points, but a radius is given -> points_within_radius
    points_within_radius(src, dst, radius)
  } else {
    abort(
      "Either a radius, number of points, or both must be provided.",
      class = "group_method_error"
    )
  }
}


#' Returns the n closest points of interest around each point in source
#'
#' @noRd
n_nearest_points <- function(src, dst, n, group_index = NULL) {
  select_lowest_distance <- function(n, m, dmat, smat) {
    if (n > length(dst)) {
      return(data_frame())
    }
    index <- which(dmat[m, ] %in% smat[m, n])
    sel <- dst[index]
    group <- data.frame(.group = group_index %||% m)
    cbind(group, sel)
  }

  # Create a distance matrix and sort it row-wise by distance
  dist_matrix <- unclass(sf::st_distance(src, dst))
  sorted_matrix <- t(apply(dist_matrix, MARGIN = 1, sort.int, method = "quick"))

  # For each source point, select the n points with the lowest distance
  iter <- expand.grid(n = seq_len(n), m = seq_len(length(src)))
  args <- list(dmat = dist_matrix, smat = sorted_matrix)
  output <- Map(select_lowest_distance, iter[["n"]], iter[["m"]], MoreArgs = args)
  sf::st_as_sf(as_data_frame(do.call(rbind.data.frame, output)))
}


#' Returns points of interest that lie within a specified radius around each
#' point in source
#'
#' @noRd
points_within_radius <- function(src, dst, radius) {
  buffers <- sf::st_buffer(src, radius)
  within <- sf::st_within(dst, buffers, sparse = FALSE)
  sel <- lapply(seq_len(ncol(within)), \(i) {
    cbind(
      data_frame(.group = rep.int(i, sum(within[, i]))),
      dst[within[, i]]
    )
  })
  sel <- sf::st_as_sf(as_data_frame(do.call(rbind.data.frame, sel)))
  sel
}
