ors_cache <- new.env(parent = emptyenv())


recover_from_cache <- function(obj, force = FALSE) {
  obj <- deparse(substitute(obj))
  obj <- get0(obj, envir = ors_cache)
  if (!is.null(obj) && !force) {
    return_from_parent(obj, .envir = parent.frame())
  }
}


#' Utility functions
#'
#' Utility functions to aid the setup of local instances.
#' \itemize{
#'  \item \code{get_instance} checks for the existence of a mounted instance in
#'  the current session and returns it.
#'  \item \code{get_status} returns the status reported by the ORS server.
#'  \item \code{ors_ready} checks if the mounted service is ready to use.
#'  \item \code{get_profiles} is a wrapper around \code{get_status} that returns
#'  the active profiles of the mounted service.
#' }
#'
#' @param url \code{[character]}
#'
#' URL of an ORS server. Defaults to the the URL of the currently mounted
#' ORS instance. This argument exists as a way to explicitly specify the URL
#' to query in case the instance cannot easily be determined. For normal use,
#' this argument should not need to be specfied.
#' @param force \code{[logical]}
#'
#' If \code{TRUE}, function must query server. If \code{FALSE}, the information
#' will be read from the cache if possible. This argument is specially useful
#' automated workflows where it is inconvenient to query the server
#' unnecessarily often.
#' @param error \code{[logical]}
#'
#' If \code{TRUE}, gives out an error if the service is not ready.
#'
#' @returns \code{get_instance} returns an object of class \code{ors_instance}.
#' \code{get_status} returns a list of information on the running service.
#' \code{ors_ready} returns a length-1 logical vector specifying if the service
#' is running. \code{get_profiles} returns a vector containing the active
#' profiles.
#'
#' @seealso \code{\link{ors_instance}}
#' @export
#'
#' @examples
#' # initialize an ORS instance
#' ors <- ors_instance()
#'
#' # retrieve the instance object
#' get_instance()
#'
#' # check if the service is ready
#' ors_ready()
#'
#' # assert the ready status
#' try(ors_ready(error = TRUE))
#'
#' \dontrun{
#' # the following functions require a running service
#' # retrieve a list of service options from the server
#' get_status()
#' }
#'
get_instance <- function() {
  if (any_mounted()) {
    get("instance", envir = ors_cache)
  } else {
    code <- cli::code_highlight("ors_instance()")
    msg <- c(
      "No OpenRouteService instance initialized.",
      "i" = "You can initialize an instance using {code}"
    )
    abort(msg, class = "init_error")
  }
}


check_instance <- function(instance = NULL) {
  instance <- instance %||% get_instance()
  assert_that(inherits(instance, "ORSLocal"))
  instance
}


#' Returns the output of `docker inspect` as parsed json
#' @noRd
inspect_container <- function(id = NULL) {
  recover_from_cache(container_info)

  if (is.null(id)) {
    return()
  }

  assert_docker_running()
  cmd <- c("container", "inspect", id)

  container_info <- callr::run(
    command = "docker",
    args = cmd,
    stdout = "|",
    stderr = NULL,
    error_on_status = FALSE
  )

  if (length(container_info$stderr)) {
    container_info$stderr <- gsub("Error: ", "", container_info$stderr)
    abort(
      "Cannot access container {.val {name}}: {container_info$stderr}",
      "container_inaccessible_error"
    )
  }

  container_info <- jsonlite::fromJSON(container_info$stdout)
  assign("container_info", container_info, envir = ors_cache)
  container_info
}


#' @rdname get_instance
#' @export
get_status <- function(url = NULL) {
  url <- url %||% get_ors_url()
  ors_ready(url = url, force = TRUE, error = TRUE)
  if (!is_ors_api(url)) {
    req <- httr2::request(url)
    req <- httr2::req_template(req, "GET ors/v2/status")
    res <- httr2::req_perform(req, verbosity = 0L)
    res <- httr2::resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)
    class(res) <- "ors_status"
    res
  } else {
    unlist(base_profiles(), use.names = FALSE)
  }
}


#' @rdname get_instance
#' @export
get_profiles <- function(url = NULL, force = TRUE) {
  recover_from_cache(profiles, force = force)
  profiles <- get_status(url)

  if (is.list(profiles)) {
    profiles <- lapply(profiles$profiles, function(x) x$profiles)
    profiles <- unlist(profiles, use.names = FALSE)
  }

  assign("profiles", profiles, envir = ors_cache)
  profiles
}


#' @rdname get_instance
#' @export
ors_ready <- function(url = NULL, force = TRUE, error = FALSE) {
  ready <- get0("ready", envir = ors_cache)
  if (isFALSE(ready)) {
    recover_from_cache(ready, force = force)
  }

  url <- url %||% get_ors_url()
  ready <- TRUE
  if (!is_ors_api(url)) {
    req <- httr2::request(url)
    req <- httr2::req_template(req, "GET ors/v2/health")
    tryCatch(
      expr = {
        res <- httr2::req_perform(req)
        res <- httr2::resp_body_json(res)
        ready <- res$status == "ready"
        stopifnot(ready)
      },
      error = function(e) {
        if (!error) {
          ready <<- FALSE
          return()
        }

        tip <- if (is_local(url)) {
          c("i" = "Did you start your local instance?")
        }

        abort(
          c("Cannot reach the OpenRouteService server.", tip),
          call = NULL,
          class = "unavailable_error"
        )
      }
    )
  }

  assign("ready", ready, envir = ors_cache)
  ready
}


#' Searches for a (mention of) an OSM extract inside the ORS directory.
#' Looks in three places:
#'  - source_file field of ors-config.yml
#'  - source_file field in combination with files directory
#'  - Any pbf file in files directory
#' If no extract file is found, NULL is returned
#' @noRd
identify_extract <- function(dir) {
  compose_path <- file.path(dir, "docker-compose.yml")
  config_path <- file.path(dir, "ors-config.yml")
  is_docker <- file.exists(compose_path)
  file_path <- ifelse(is_docker, "ors-docker/files", "")
  file_path <- file.path(dir, file_path)
  extract_path <- ""

  if (!file.exists(extract_path) && file.exists(config_path)) {
    config <- read_ors_yaml(config_path)
    src_file <- config$ors$engine$source_file
    if (!is.null(src_file)) {
      extract_path <- file.path(file_path, extract_path)
    }
  }

  if (!file.exists(extract_path)) {
    data_files <- list.files(file_path, full.names = TRUE)
    is_pbf <- is_pbf(data_files)

    if (sum(is_pbf) == 1L) {
      extract_path <- data_files[is_pbf]
    }
  }

  if (!file.exists(extract_path)) {
    extract_path <- NULL
  }

  extract_path
}


#' Returns the URL of an ORS instance
#' @noRd
get_ors_url <- function(instance = NULL) {
  instance <- instance %||% get_instance()
  instance$get_url()
}


#' Checks if an instance is stored in ors_cache
#' @noRd
any_mounted <- function() {
  "instance" %in% names(ors_cache)
}


ors_is_local <- function(instance) {
  inherits(instance, "ORSLocal")
}


assert_endpoint_available <- function(url, endpoint) {
  status <- get_status(url)
  available <- endpoint %in% status$services

  if (!available) {
    fun <- paste0("ors_", endpoint)
    msg <- "{.fn {fun}} is not available on the mounted API."
    abort(msg, class = "endpoint_unavailable_error")
  }
}
