#' Routing distance matrix
#' @description Calls the matrix service and returns a routing distance matrix.
#' @inheritParams ors_distances
#' @returns If \code{length(proximity_type) == 1}, returns a
#' \code{nrow(source) * nrow(destination)} routing distance matrix. Otherwise,
#' returns a list containing two matrices accordingly.
#'
#' @export
ors_matrix <- function(source,
                       destination,
                       profile = get_profiles(),
                       units = c("m", "km", "mi"),
                       proximity_type = c("distance", "duration"),
                       instance = NULL) {
  if (is.null(instance)) {
    instance <- get_instance()
  }
  iid <- get_id(instance = instance)
  
  # Check if ORS is ready to use
  ors_ready(force = FALSE, error = TRUE, id = iid)
  
  profile <- match.arg(profile)
  proximity_type <- match.arg(proximity_type)
  units <- match.arg(units)
  
  source <- format_input_data(source, len = nrow(destination))
  destination <- format_input_data(destination, len = nrow(source))
  
  url <- get_ors_url(id = iid)
  
  res <- call_ors_matrix(
    source = source,
    destination = destination,
    profile = profile,
    metrics = proximity_type,
    units = units,
    url = url,
    token = instance$token
  )
  
  handle_ors_conditions(res, abort_on_error = TRUE, warn_on_warning = TRUE)
  
  if (length(proximity_type) == 1L) {
    matrix <- res[[paste0(proximity_type, "s")]]
  } else {
    matrix <- list(distances = res$distances, durations = res$durations)
  }
  
  matrix
}