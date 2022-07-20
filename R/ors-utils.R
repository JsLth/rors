# Title     : Functions to get infos from ORS
# Objective : Get container or local server info such as ports, status or
#             extract location
# Created by: Jonas Lieth
# Created on: 29.09.2021


ors_cache <- new.env(parent = emptyenv())


clear_cache <- function() {
  cache_items <- ls(ors_cache)
  remove(list = cache_items, envir = ors_cache)
}


get_instance <- function() {
  instance <- get("instance", envir = ors_cache)
  
  if (is.null(instance)) {
    cli::cli_abort(c(
      "No OpenRouteService instance initialized.",
      "i" = "You can initialize an instance using {.code ors_instance}"
    ))
  }
  
  instance
}


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


get_status <- function(id = NULL) {
  id <- get_id(id)
  ors_ready(id = id, force = TRUE, error = TRUE)
  url <- file.path(get_ors_url(id), "ors/v2/status", fsep = "/")
  if (!is_ors_api(url)) {
    url <- url
    status_res <- httr::content(
      httr::GET(url),
      as = "text",
      type = "application/json",
      encoding = "UTF-8"
    )
    jsonlite::fromJSON(status_res, simplifyVector = TRUE, flatten = TRUE)
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
#' @param force \code{[logical]}
#' 
#' If \code{TRUE}, function must query the local host. If
#' \code{FALSE}, profiles will be read from the cache if possible.
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
#' @param id [character]
#' ID or name of a container or URL of a server that should be checked. If NULL,
#' retrieves the ID from the current instance set by
#' \code{\link[ORSRouting]{ors_instance}}
#' @param force [logical]
#' If \code{TRUE}, function must query server. If \code{FALSE}, the status will
#' be read from the cache if possible.
#' @param error [logical]
#' If \code{TRUE}, gives out an error if the service is not ready.
#'
#' @export

ors_ready <- function(id = NULL, force = TRUE, error = FALSE) {
  if (is.null(ors_cache$ors_ready) || isFALSE(ors_cache$ors_ready) || force) {
    id <- get_id(id)
    url <- file.path(get_ors_url(id), "ors/health", fsep = "/")
    
    if (!is_ors_api(url)) {
      ready <- tryCatch(
        expr = {
          res <- httr::GET(url)
          res <- jsonlite::fromJSON(
            httr::content(
              res,
              as = "text",
              type = "application/json",
              encoding = "UTF-8"
            )
          )
          stopifnot(r <- identical(res$status, "ready"))
          r
        },
        error = function(e) {
          if (isTRUE(error)) {
            e <- gsub("Error", "Cause", as.character(e))
            cli::cli_abort(c("ORS service is not reachable.", e))
          } else FALSE
        }
      )
    } else ready <- TRUE

    assign("ors_ready", ready, envir = ors_cache)
    ready
  } else {
    ors_cache$ors_ready
  }
}


get_ors_dir <- function(force = TRUE, id = NULL) {
  if (is.null(ors_cache$mdir) || isTRUE(force)) {
    container_info <- inspect_container(id)
    
    mdir <- container_info$Config$Labels$com.docker.compose.project.working_dir
    mdir <- unlist(strsplit(normalizePath(mdir, winslash = "/"), "/"))
    mdir <- paste0(utils::head(mdir, -1L), collapse = "/")

    if (!dir.exists(mdir)) {
      cli::cli_abort("The current ORS container directory does not exist")
    }

    assign("mdir", mdir, envir = ors_cache)
    mdir
  } else {
    ors_cache$mdir
  }
}


identify_extract <- function(force = FALSE, id = NULL) {
  if (is.null(ors_cache$extract_path) || force) {
    # Save docker working directory
    mdir <- get_ors_dir(id = id)

    # Read extract file location from compose file
    compose <- yaml::yaml.load_file(file.path(mdir, "docker/docker-compose.yml"))
    extract_path <- compose$services$`ors-app`$build$args$OSM_FILE

    # Check if build argument is set
    if (is.null(extract_path)) {
      volume <- compose$services$`ors-app`$volumes[6L]
      extract_path <- gsub("\\./", "", strsplit(volume, ":")[[1L]][1L])

      # If not, check if change volume is set
      if (is.null(extract_path) || is.na(extract_path)) {
        data_dir <- file.path(mdir, "docker/data")
        osm_file_occurences <- grepl(".pbf|.osm.gz|.osm.zip|.osm", dir(data_dir))

        # As a last resort, check if we can just pick it up from the data folder
        if (sum(osm_file_occurences) == 1L) {
          extract_path <- dir(data_dir)[osm_file_occurences]
        } else {
          cli::cli_abort(paste("Cannot identify current extract file.",
                               "Pass it explicitly."))
        }
      }
    }

    # Convert relative to absolute path
    extract_path <- normalizePath(
      file.path(
        mdir,
        "docker/data",
        basename(extract_path)
      ), winslash = "/"
    )

    assign("extract_path", extract_path, envir = ors_cache)
    extract_path
  } else {
    ors_cache$extract_path
  }
}


get_id <- function(id = NULL, instance = NULL) {
  if (is.null(id)) {
    if (is.null(instance)) {
      instance <- get_instance()
    }
    
    if (attr(instance, "type") == "local") {
      id <- instance$compose$name
    } else if (attr(instance, "type") == "remote") {
      id <- instance$url
    } else {
      corrupt_instance(instance)
    }
  } else id
}


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


get_ors_url <- function(id = NULL) {
  if (!is_url(id)) {
    sprintf("http://localhost:%s/", get_ors_port(id = id))
  } else id
}


get_ors_host <- function() {
  url <- get_ors_url()
  if (is_local(url)) {
    regex_match(url, "([[:alnum:]\\.]+)(?:\\:[[:digit:]]+)")[[1]][2]
  } else NULL
}


any_mounted <- function() {
  "instance" %in% names(ors_cache)
}


corrupt_instance <- function(instance) {
  nam <- substitute(instance, env = parent.frame(3))
  cli::cli_abort(c(
    "OpenRouteService instance {.var {nam}} might be corrupt.",
    "i" = "You can re-initialize the instance using {.code ors_instance}."
  ))
}


#' Return ORS conditions
#' @description Return the error and warning messages that ORS returned in the
#' last \code{\link{ors_distances}} function call. Also works for
#' \code{\link[ORSRouting]{ors_shortest_distances}}.
#' @param last \code{[integer]}
#' 
#' Number of error lists that should be returned. \code{last = 2L},
#' for example, returns errors from the last two function calls.
#'
#' @export

last_ors_conditions <- function(last = 1L) {
  conditions <- ors_cache$routing_conditions

  if (length(conditions)) {
    cli_abortifnot(is.numeric(last))
    last <- min(last, length(conditions))

    time <- names(conditions)

    cond_df <- lapply(conditions, function(x) {
      stats::na.omit(data.frame(conditions = unlist(x)))
    })
    names(cond_df) <- time

    end <- length(names(cond_df))
    start <- length(names(cond_df)) + 1L - last
    selected_conditions <- cond_df[seq(start, end)]

    class(selected_conditions) <- "ors_condition"
    selected_conditions
  }
}
