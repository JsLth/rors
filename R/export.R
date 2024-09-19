#' Export graphs
#'
#' @description
#' Retrieve the base graphs for different modes of transport. Includes all
#' nodes and edges that are used for routing.
#'
#' @param bbox \code{[various]}
#'
#' An object created by \code{\link[sf]{st_bbox}} or any object that can be
#' coerced to a bbox object. Only graphs within these boundaries will be
#' exported.
#'
#' @param network \code{[logical]}
#'
#' Whether to convert the output to an \code{\link[sfnetworks]{sfnetwork}}.
#'
#' @param ... Additional arguments passed to the export API.
#' @inheritParams ors_pairwise
#' @returns If \code{sfnetworks} is installed, returns an
#' \code{\link[sfnetworks]{sfnetwork}}. Otherwise, returns a list with
#' fields \code{nodes} and \code{edges}. The nodes network contains a column
#' \code{weight} that represents the fastest routing duration with the
#' specified profile.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sfnetworks)
#' library(ggplot2)
#'
#' data("pharma")
#'
#' # export graphs as sfnetwork
#' exp1 <- ors_export(pharma, network = TRUE)
#' plot(exp1)
#'
#' # export graphs as list
#' exp2 <- ors_export(pharma, network = FALSE)
#'
#' ggplot(exp2$edges) +
#'   geom_sf(aes(lwd = weight))
#'
#' ggplot() +
#'   geom_sf(data = exp2$nodes, size = 1) +
#'   geom_sf(data = exp2$edges)}
ors_export <- function(bbox,
                       profile = get_profiles(),
                       network = TRUE,
                       instance = NULL,
                       ...) {
  instance <- check_instance(instance)
  url <- get_ors_url(instance)
  assert_that(is.logical(network))
  bbox <- sf::st_bbox(bbox)
  profile <- match.arg(profile)

  if (is_ors_api(url)) {
    abort(
      "{.fn ors_export} is not supported for the public API.",
      class = "public_export_error"
    )
  }

  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, url = url)

  res <- call_ors_export(bbox, profile, url, ...)
  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)

  export <- tidy_export(res, network = network)
}


tidy_export <- function(res, network) {
  # Construct nodes
  nodes <- res$nodes
  geom <- sf::st_sfc(lapply(nodes$location, sf::st_point), crs = 4326)
  nodes <- sf::st_sf(data_frame(geometry = geom))

  if (!nrow(nodes)) {
    msg <- c(
      "!" = "Street network is empty.",
      "i" = "Ensure that the specified boundary box is valid and covers streets."
    )
    abort(msg, class = "empty_export_error")
  }

  # Construct edges
  edges <- res$edges
  from <- match(edges$fromId, res$nodes$nodeId)
  to <- match(edges$toId, res$nodes$nodeId)
  geom <- Map(
    function(from, to) {
      geom <- sf::st_geometry(nodes[c(from, to), ])
      geom <- sf::st_combine(geom)
      sf::st_cast(geom, "LINESTRING")
    },
    from = from,
    to = to
  )

  geom <- do.call(c, geom)
  edges <- sf::st_sf(data_frame(
    weight = edges$weight,
    from = from,
    to = to,
    geometry = geom
  ))

  if (loadable("sfnetworks") && network) {
    sfnetworks::sfnetwork(nodes = nodes, edges = edges)
  } else {
    list(nodes = nodes, edges = edges)
  }
}
