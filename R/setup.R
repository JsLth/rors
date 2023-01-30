#' Local OpenRouteService setup
#' 
#' @description Performs a complete initial OpenRouteService server setup.
#' This function is a wrapper around \code{\link{ors_instance}},
#' \code{\link{ors_extract}}, \code{\link{ors_config}},
#' \code{\link{ors_settings}} and \code{\link{ors_up}} that simply serves
#' convenience. For a deeper understanding of what this function does, refer
#' to the corresponding function documentations.
#' 
#' After completing the setup, an instance object is returned and mounted to
#' the session. Functions that require OpenRouteService will automatically
#' access this object to determine where to send requests.
#' 
#' @inheritParams ors_instance
#' @inheritParams ors_extract
#' @inheritParams ors_config
#' @inheritParams ors_settings
#' @inheritParams ors_up
#' @param ... Further arguments passed to \code{\link{ors_config}}.
#' 
#' @returns Nested list of class \code{ors_instance}.
#' @export
ors_setup <- function(dir = "~",
                      version = "master",
                      overwrite = FALSE,
                      place = NULL,
                      provider = "geofabrik",
                      profiles = NULL,
                      ...,
                      name = NULL,
                      port = NULL,
                      memory = NULL,
                      auto_deletion = FALSE,
                      wait = TRUE,
                      tag = "latest",
                      verbose = TRUE) {
  instance <- ors_instance(
    dir = dir,
    version = version,
    overwrite = overwrite,
    verbose = verbose
  )
  
  if (!missing(place)) {
    instance <- ors_extract(
      instance,
      place = place,
      provider = provider
    )
  }
  
  if (!missing(profiles) || ...length()) {
    instance <- ors_config(instance, profiles = profiles, ...)
  }
  
  if (!missing(name) || !missing(port) || !missing(memory) || !missing(auto_deletion)) {
    instance <- ors_settings(
      instance,
      name = name,
      port = port,
      memory = memory,
      auto_deletion = auto_deletion
    )
  }
  
  instance <- ors_up(instance, wait = wait, tag = tag)
  
  instance
}