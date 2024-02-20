ors_cache <- new.env(parent = emptyenv())


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
#' @param id \code{[character]}
#'
#' ID or name of a container or URL of a server that is to be checked. If
#' \code{NULL}, retrieves the ID from the current instance set by
#' \code{\link{ors_instance}}
#' @param force \code{[logical]}
#'
#' If \code{TRUE}, function must query server. If \code{FALSE}, the information
#' will be read from the cache if possible.
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
get_instance <- function() {
  if (any_mounted()) {
    get("instance", envir = ors_cache)
  } else {
    cli::cli_abort(c(
      "No OpenRouteService instance initialized.",
      "i" = "You can initialize an instance using {.code ors_instance}"
    ))
  }
}


check_instance <- function(instance = NULL) {
  if (is.null(instance)) {
    instance <- get_instance()
  } else {
    assert_that(inherits(instance, "ORSLocal"))
  }

  instance
}


#' Returns the name of an ORS instance
#' @noRd
get_id <- function(id = NULL, instance = NULL) {
  if (is.null(id)) {
    instance <- instance %||% get_instance()

    if (inherits(instance, "ORSLocal")) {
      id <- instance$compose$name
    } else if (inherits(instance, "ORSRemote")) {
      id <- instance$url
    }
  } else {
    id
  }
}


#' Returns the output of `docker inspect` as parsed json
#' @noRd
inspect_container <- function(id = NULL) {
  if (is.null(ors_cache$container_info)) {
    if (is.null(id)) {
      return()
    }

    if (!docker_running()) {
      cli::cli_abort("Docker is not running. Cannot inspect container.")
    }

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
      cli::cli_abort("Cannot access container {.val {name}}: {container_info$stderr}")
    }

    container_info <- jsonlite::fromJSON(container_info$stdout)

    assign("container_info", container_info, envir = ors_cache)

    invisible(container_info)
  } else {
    invisible(ors_cache$container_info)
  }
}


#' @rdname get_instance
#' @export
get_status <- function(id = NULL) {
  id <- get_id(id)
  ors_ready(id = id, force = TRUE, error = TRUE)
  url <- paste0(get_ors_url(id), "ors/v2/status")
  if (!is_ors_api(url)) {
    url <- url
    req <- httr2::request(url)
    req <- httr2::req_method(req, "GET")
    req <- httr2::req_error(
      req,
      is_error = \(res) if (res$status_code == 200L) FALSE else TRUE
    )
    res <- httr2::req_perform(req, verbosity = 0L)
    res <- httr2::resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)
    class(res$profiles) <- "stprof"
    res
  } else {
    unlist(base_profiles, use.names = FALSE)
  }
}


#' @rdname get_instance
#' @export
get_profiles <- function(id = NULL, force = TRUE) {
  if (is.null(ors_cache$profiles) || isTRUE(force)) {
    id <- get_id(id)
    profiles <- get_status(id)

    if (is.list(profiles)) {
      profiles <- lapply(
        profiles$profiles,
        function(x) x$profiles
      )
      profiles <- unlist(profiles, use.names = FALSE)
    }

    assign("profiles", profiles, envir = ors_cache)
    profiles
  } else {
    ors_cache$profiles
  }
}


#' @rdname get_instance
#' @export
ors_ready <- function(id = NULL, force = TRUE, error = FALSE) {
  if (is.null(ors_cache$ors_ready) || isFALSE(ors_cache$ors_ready) || force) {
    id <- get_id(id)
    url <- paste0(get_ors_url(id), "ors/v2/health")

    if (!is_ors_api(url)) {
      req <- httr2::request(url)
      req <- httr2::req_method(req, "GET")
      tryCatch(
        expr = {
          res <- httr2::req_perform(req)
          res <- httr2::resp_body_json(res)
          stopifnot(ready <- res$status == "ready")
        },
        error = function(e) {
          if (error) {
            if (is_local(url)) {
              tip <- c("i" = "Did you start your local instance?")
            } else {
              tip <- NULL
            }

            cli::cli_abort(c("Cannot reach the OpenRouteService server.", tip))
          } else {
            ready <<- FALSE
          }
        }
      )
    } else {
      ready <- TRUE
    }

    assign("ors_ready", ready, envir = ors_cache)
    ready
  } else {
    ors_cache$ors_ready
  }
}


#' Searches for a (mention of) an OSM extract inside the ORS directory.
#' Looks in three places:
#'  - Build argument in docker compose
#'  - Data volume in docker compose
#'  - Data directory
#' If no extract file is found, NULL is returned
#' @noRd
identify_extract <- function(compose, dir) {
  # Read extract file location from compose file
  extract_path <- compose$services$`ors-app`$build$args$OSM_FILE
  extract_path <- gsub("\\./", "", extract_path)
  extract_path <- file.path(dir, extract_path)

  # Check if build argument is set
  if (!length(extract_path) || !is_pbf(extract_path)) {
    volume <- utils::tail(compose$services$`ors-app`$volumes, 1)
    extract_path <- gsub("\\./", "", strsplit(volume, ":")[[1L]][1L])
    extract_path <- file.path(dir, extract_path, fsep = "/")
    if (is.na(extract_path) || !is_pbf(extract_path)) extract_path <- NULL

    # If not, check if change volume is set
    if (is.null(extract_path)) {
      data_dir <- file.path(dir, "docker/data")
      data_files <- list.files(data_dir, full.names = TRUE)
      is_pbf <- is_pbf(data_files)

      # As a last resort, check if we can just pick it up from the data folder
      if (sum(is_pbf) == 1L) {
        extract_path <- data_files[is_pbf]
      }
    }
  }

  extract_path
}


#' Returns the host port of an ORS instance
#' @noRd
get_ors_port <- function(force = FALSE, id = NULL) {
  if (!is.null(id) || is.null(ors_cache$instance) || force) {
    container_info <- inspect_container(id)

    port <- container_info$HostConfig$PortBindings[[1]][[1]]$HostPort
    assign("port", port, envir = ors_cache)
    port
  } else {
    ors_cache$instance$compose$ports[1, 1]
  }
}


#' Returns the URL of an ORS instance
#' @noRd
get_ors_url <- function(id = NULL) {
  id <- id %||% get_id()
  if (!is_url(id)) {
    sprintf("http://localhost:%s/", get_ors_port(id = id))
  } else {
    if (!endsWith(id, "/")) {
      paste0(id, "/")
    }
    id
  }
}


#' Checks if an instance is stored in ors_cache
#' @noRd
any_mounted <- function() {
  "instance" %in% names(ors_cache)
}


ors_is_local <- function(instance) {
  inherits(instance, "ORSLocal")
}
