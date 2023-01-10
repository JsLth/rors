# Title     : Functions to get infos from ORS
# Objective : Get container or local server info such as ports, status or
#             extract location
# Created by: Jonas Lieth
# Created on: 29.09.2021


ors_cache <- new.env(parent = emptyenv())


#' Checks if an instance is stored in ors_cache and, if so, returns it
#' @noRd
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
    assert_that(inherits(instance, "ors_instance"))
  }
  
  instance
}


#' Returns the name of an ORS instance
#' @noRd
get_id <- function(id = NULL, instance = NULL) {
  if (is.null(id)) {
    if (is.null(instance)) {
      instance <- get_instance()
    }

    if (attr(instance, "type") == "local") {
      id <- instance$compose$name
    } else if (attr(instance, "type") == "remote") {
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


#' Returns the content of /ors/status as parsed json
#' @noRd
get_status <- function(id = NULL) {
  id <- get_id(id)
  ors_ready(id = id, force = TRUE, error = TRUE)
  url <- file.path(get_ors_url(id), "ors/v2/status", fsep = "/")
  if (!is_ors_api(url)) {
    url <- url
    req <- httr2::request(url)
    req <- httr2::req_method(req, "GET")
    req <- httr2::req_error(
      req,
      is_error = \(res) if (res$status_code == 200L) FALSE else TRUE
    )
    res <- httr2::req_perform(req, verbosity = 0L)
    httr2::resp_body_json(res, simplifyVector = TRUE, flatten = TRUE)
  } else {
    list(profiles = list(
      "profile 1" = list(profiles = "driving-car"),
      "profile 2" = list(profiles = "driving-hgv"),
      "profile 3" = list(profiles = "cycling-regular"),
      "profile 4" = list(profiles = "cycling-mountain"),
      "profile 5" = list(profiles = "cycling-electric"),
      "profile 6" = list(profiles = "cycling-road"),
      "profile 7" = list(profiles = "foot-walking"),
      "profile 8" = list(profiles = "foot-hiking"),
      "profile 9" = list(profiles = "wheelchair")
    ))
  }
}


#' Get active ORS profiles
#' @description Returns a list of active profiles from the cache or local host.
#' Requires OpenRouteService to be running.
#' @param force \code{[logical]}
#'
#' If \code{TRUE}, function must query the local host. If
#' \code{FALSE}, profiles will be read from the cache if possible.
#' @inheritParams ors_ready
#'
#' @export
get_profiles <- function(id = NULL, force = TRUE) {
  if (is.null(ors_cache$profiles) || isTRUE(force)) {
    id <- get_id(id)
    ors_info <- get_status(id)
    profiles <- lapply(
      ors_info$profiles,
      function(x) x$profiles
    )
    profiles <- unlist(profiles, use.names = FALSE)
    assign("profiles", profiles, envir = ors_cache)
    profiles
  } else {
    ors_cache$profiles
  }
}


#' Is ORS usable?
#' @description States whether the ORS service is set up and ready to use.
#' @param id \code{[character]}
#' 
#' ID or name of a container or URL of a server that is to be checked. If
#' \code{NULL}, retrieves the ID from the current instance set by
#' \code{\link{ors_instance}}
#' @param force \code{[logical]}
#' 
#' If \code{TRUE}, function must query server. If \code{FALSE}, the status will
#' be read from the cache if possible.
#' @param error \code{[logical]}
#' 
#' If \code{TRUE}, gives out an error if the service is not ready.
#'
#' @export
ors_ready <- function(id = NULL, force = TRUE, error = FALSE) {
  if (is.null(ors_cache$ors_ready) || isFALSE(ors_cache$ors_ready) || force) {
    id <- get_id(id)
    url <- file.path(get_ors_url(id), "ors/health", fsep = "/")

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
            cli::cli_abort(c(
              "x" = "Cannot reach the OpenRouteService server.",
              "i" = "Did you start your local instance?"
            ), call = parent.frame(4))
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
#' First checks the compose file, then looks in the data path.
#' @noRd
identify_extract <- function(instance) {
  # Read extract file location from compose file
  compose <- instance$compose$parsed
  mdir <- instance$paths$dir
  extract_path <- compose$services$`ors-app`$build$args$OSM_FILE

  # Check if build argument is set
  if (is.null(extract_path)) {
    volume <- compose$services$`ors-app`$volumes[6L]
    extract_path <- gsub("\\./", "", strsplit(volume, ":")[[1L]][1L])
    if (is.na(extract_path)) extract_path <- NULL

    # If not, check if change volume is set
    if (is.null(extract_path)) {
      data_dir <- file.path(mdir, "docker/data")
      osm_file_occurences <- grepl("\\.pbf$|\\.osm.gz$|\\.osm\\.zip$|\\.osm$", dir(data_dir))

      # As a last resort, check if we can just pick it up from the data folder
      if (sum(osm_file_occurences) == 1L) {
        extract_path <- dir(data_dir, full.names = TRUE)[osm_file_occurences]
      }
    }
  }

  if (!is.null(extract_path)) {
    # Convert relative to absolute path
    extract_path <- normalizePath(
      file.path(
        instance$paths$dir,
        "docker/data",
        basename(extract_path)
      ),
      winslash = "/"
    )
  }

  extract_path
}


#' Returns the host port of an ORS instance
#' @noRd
get_ors_port <- function(force = FALSE, id = NULL) {
  if (is.null(ors_cache$instance) || force == TRUE) {
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
  if (!is_url(id)) {
    sprintf("http://localhost:%s/", get_ors_port(id = id))
  } else {
    id
  }
}


#' Checks if an instance is stored in ors_cache
#' @noRd
any_mounted <- function() {
  "instance" %in% names(ors_cache)
}
