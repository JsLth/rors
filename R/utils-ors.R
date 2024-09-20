cache_env <- new.env(parent = emptyenv())


rors_cache <- function() {
  get("cache_env", envir = asNamespace("rors"))
}


#' Get an object from cache and return it from the parent environment
#' @noRd
recover_from_cache <- function(obj, force = FALSE) {
  obj <- get0(obj, envir = rors_cache())
  if (!is.null(obj) && !force) {
    obj <- obj[[1]]
    return_from_parent(obj, .envir = parent.frame())
  }
}


#' Store an object in the rors cache environment as a runtime object
#' @noRd
store_in_cache <- function(obj) {
  name <- deparse(substitute(obj))
  obj <- list(obj)
  class(obj) <- "rors_runtime_cache"
  assign(name, obj, envir = rors_cache())
}


#' Remove all runtime objects from the rors cache
#' @noRd
clear_runtime_cache <- function() {
  is_runtime <- eapply(rors_cache(), function(x) {
    inherits(x, "rors_runtime_cache")
  })
  cached <- names(is_runtime)[unlist(is_runtime)]
  rm(list = cached, envir = rors_cache())
}


#' Utility functions
#'
#' Utility functions to aid the setup of local instances.
#' \itemize{
#'  \item \code{get_instance} checks for the existence of a mounted instance in
#'  the current session and returns it.
#'  \item \code{ors_status} returns the status reported by the ORS server.
#'  \item \code{ors_ready} checks if the mounted service is ready to use.
#'  \item \code{get_profiles} is a wrapper around \code{get_status} that returns
#'  the active profiles of the mounted service.
#'  \item \code{any_mounted} checks if an instance is mounted to the current
#'  session.
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
#' unnecessarily often. Functions like \code{\link{ors_pairwise}} or
#' \code{\link{ors_inspect}} generally do not force server requests for these
#' functions in order to make subsequent calls faster. When running directly,
#' this argument should stay \code{TRUE} to ensure the accuracy of the output.
#' @param error \code{[logical]}
#'
#' If \code{TRUE}, gives out an error if the service is not ready.
#'
#' @returns \code{get_instance} returns an object of class \code{ors_instance}.
#' \code{ors_status} returns a list of information on the running service.
#' \code{ors_ready} returns a length-1 logical vector specifying if the service
#' is running. \code{get_profiles} returns a vector containing the active
#' profiles. \code{any_mounted} returns a length-1 logical.
#'
#' @seealso \code{\link{ors_instance}}
#' @export
#'
#' @section Caching:
#'
#' The following functions make use of a runtime caching system:
#' \code{ors_ready()}, \code{ors_status()}, \code{get_profiles()},
#' \code{get_extract_boundaries()}, \code{ors_sample()}. This means that,
#' if \code{force = FALSE}, previously generated output is re-used instead
#' of sending new requests. This can be particularly useful in automated
#' workflows like loops where speed is important. When run directly, caching
#' should not be necessary, which is why \code{force = FALSE} is the default
#' of most of these functions (except \code{get_extract_boundaries()} because
#' it deals with potentially much larger amounts of data).
#'
#' "Runtime" in this context refers to the runtime of an ORS instance, i.e.
#' the time after it is started. Cached results should only be valid for a
#' specific runtime and discarded afterwards. When mounting a new instance
#' different from the one mounted before, the runtime cache is cleared
#' so that fresh requests must be made.
#'
#' @examples
#' # initialize an ORS instance
#' ors <- ors_instance(dir = tempdir(), dry = TRUE)
#'
#' # confirm that instance was mounted
#' any_mounted() # TRUE
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
#' ors_status()
#'
#' # retrieve a list of active profiles that can be used
#' get_profiles()
#' }
get_instance <- function() {
  if (any_mounted()) {
    get("instance", envir = rors_cache())
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
  assert_that(inherits(instance, "ORSInstance"))
  instance
}


#' @rdname get_instance
#' @export
ors_status <- function(url = NULL, force = TRUE) {
  recover_from_cache("status", force = force)
  url <- url %||% get_ors_url()
  ors_ready(url = url, force = FALSE, error = TRUE)
  if (!is_ors_api(url)) {
    req <- httr2::request(url)
    req <- httr2::req_template(req, "GET ors/v2/status")
    status <- perform_call(req)
  } else {
    status <- list(
      profiles = unlist(base_profiles(), use.names = FALSE),
      services = list("routing", "matrix", "isochrones", "snap")
    )
  }
  class(status) <- "ors_status"
  store_in_cache(status)
  status
}


#' @rdname get_instance
#' @export
get_profiles <- function(url = NULL, force = TRUE) {
  profiles <- ors_status(url, force = force)$profiles

  if (is.list(profiles)) {
    profiles <- lapply(profiles, function(x) x$profiles)
    profiles <- unlist(profiles, use.names = FALSE)
  }

  profiles
}


#' @rdname get_instance
#' @export
ors_ready <- function(url = NULL, force = TRUE, error = FALSE) {
  ready <- get0("ready", envir = rors_cache())[[1]]
  if (isTRUE(ready)) {
    recover_from_cache("ready", force = force)
  }

  url <- url %||% get_ors_url()
  ready <- TRUE
  if (!is_ors_api(url)) {
    req <- httr2::request(url)
    req <- httr2::req_template(req, "GET ors/v2/health")
    tryCatch(
      expr = {
        res <- perform_call(req)
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

  store_in_cache(ready)
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
  file_path <- ifelse(is_docker, "files", "")
  file_path <- file.path(dir, file_path)
  extract_path <- ""

  if (!file.exists(extract_path) && file.exists(config_path)) {
    config <- read_ors_yaml(config_path)
    src_file <- config$ors$engine$source_file
    if (!is.null(src_file)) {
      extract_path <- file.path(dir, src_file)
    }
  }

  if (!file.exists(extract_path)) {
    data_files <- list.files(file_path, full.names = TRUE)
    is_pbf <- is_pbf(data_files)

    if (sum(is_pbf) == 1L) {
      extract_path <- data_files[is_pbf] # nocov
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


#' @rdname get_instance
#' @export
any_mounted <- function() {
  "instance" %in% names(rors_cache())
}


ors_is_local <- function(instance) {
  inherits(instance, "ORSLocal")
}


assert_endpoint_available <- function(url, endpoint) {
  status <- ors_status(url, force = FALSE)
  available <- endpoint %in% status$services

  if (!available) {
    fun <- sys.call(sys.parent())[[1]] # nocov start
    msg <- "{.fn {fun}} is not available on the mounted API."
    abort(msg, class = "endpoint_unavailable_error") # nocov end
  }
}
