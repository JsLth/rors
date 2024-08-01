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
    code <- cli::code_highlight("ors_instance()")
    cli::cli_abort(
      c(
        "No OpenRouteService instance initialized.",
        "i" = "You can initialize an instance using {code}"
      ),
      class = "ors_init_error"
    )
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
  }

  id
}


#' Returns the output of `docker inspect` as parsed json
#' @noRd
inspect_container <- function(id = NULL) {
  if (is.null(ors_cache$container_info)) {
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
      cli::cli_abort(
        "Cannot access container {.val {name}}: {container_info$stderr}",
        "ors_container_inaccessible_error"
      )
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
    class(res) <- "ors_status"
    res
  } else {
    unlist(base_profiles(), use.names = FALSE)
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

            cli::cli_abort(
              c("Cannot reach the OpenRouteService server.", tip),
              call = NULL,
              class = "ors_unavailable_error"
            )
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
#'  - source_file field of ors-config.yml
#'  - source_file field in combination with files directory
#'  - Any pbf file in files directory
#' If no extract file is found, NULL is returned
#' @noRd
identify_extract <- function(dir) {
  compose_path <- file.path(dir, "docker-compose.yml")
  config_path <- file.path(dir, "ors-config.yml")
  extract_path <- ""

  if (file.exists(compose_path) && file.exists(config_path)) {
    config <- read_ors_yaml(config_path)
    extract_file <- basename(config$ors$engine$source_file)
    extract_path <- file.path(dir, "ors-docker", "files", extract_file)
  }

  if (!file.exists(extract_path) && file.exists(config_path)) {
    config <- read_ors_yaml(config_path)
    extract_path <- config$ors$engine$source_file %||% ""
    extract_path <- file.path(dir, extract_path)
  }

  if (!file.exists(extract_path)) {
    data_dir <- file.path(dir, "ors-docker/files")
    data_files <- list.files(data_dir, full.names = TRUE)
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


assert_endpoint_available <- function(url) {
  if (is_ors_api(url)) {
    cli::cli_abort(
      "{.fn ors_snap} is not available on the public API.",
      class = "ors_endpoint_unavailable_error"
    )
  }
}
