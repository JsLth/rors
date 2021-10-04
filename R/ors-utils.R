# Title     : Functions to get infos from ORS
# Objective : Get container or local server info such as ports, status or
#             extract location
# Created by: Jonas Lieth
# Created on: 29.09.2021


pkg_cache <- new.env(parent = emptyenv())


clear_cache <- function() {
  cache_items <- ls(pkg_cache)
  remove(list = cache_items, envir = pkg_cache)
}


get_container_info <- function() {
  if (is.null(pkg_cache$container_info)) {
    container_info <- system(
      ensure_permission(
        "docker container inspect ors-app"
      ),
      intern = TRUE
    ) %>%
      paste0(collapse = "\n") %>%
      jsonlite::fromJSON()

    if (length(container_info) == 0) {
      cli::cli_abort("No such container: ors-app")
    }

    assign("container_info", container_info, envir = pkg_cache)
    invisible(container_info)
  } else {
    invisible(pkg_cache$container_info)
  }
}


#' Get active ORS profiles
#' @description Returns a list of active profiles from the cache or local host.
#' @param force If \code{TRUE}, function must query the local host. If
#' \code{FALSE}, profiles will be read from the cache if possible.
#'
#' @export

get_profiles <- function(force = FALSE) {
  if (is.null(pkg_cache$profiles) || isTRUE(force)) {
    if (ors_ready()) {
      url <- sprintf("http://localhost:%s/ors/v2/status", get_ors_port())
    } else {
      cli::cli_abort("ORS service is not reachable.")
    }

    status_response <- httr::GET(url) %>%
      httr::content(as = "text", type = "application/json", encoding = "UTF-8")
    ors_info <- jsonlite::fromJSON(status_response)

    profiles <- sapply(ors_info$profiles, function(x) x$profiles)
    assign("profiles", profiles, envir = pkg_cache)
    return(unname(profiles))
  } else {
    return(pkg_cache$ors_info)
  }
}


#' Is ORS usable?
#' @description States whether the ORS service is set up and ready to use.
#' @param force If \code{TRUE}, function must query local host. If
#' \code{FALSE}, the status will be read from the cache if possible.
#'
#' @export

ors_ready <- function(force = TRUE) {
  if (is.null(pkg_cache$ors_ready) || force) {
    ready <- tryCatch(
      httr::GET(
        sprintf("http://localhost:%s/ors/health", get_ors_port())
      ) %>%
        content(as = "text", type = "application/json", encoding = "UTF-8") %>%
        jsonlite::fromJSON() %>%
        .$status %>%
        identical("ready"),
      error = function(e) FALSE
    )
    assign("ors_ready", ready, envir = pkg_cache)
    return(ready)
  } else {
    return(pkg_cache$ors_ready)
  }
}


get_ors_dir <- function(force = TRUE) {
  if (is.null(pkg_cache$mdir) || isTRUE(force)) {
    container_info <- get_container_info()
    mdir <- container_info$Config$Labels$com.docker.compose.project.working_dir
    mdir <- normalizePath(mdir, winslash = "/") %>%
      strsplit(mdir, "/") %>%
      unlist() %>%
      head(-1) %>%
      paste0(collapse = "/")

    assign("mdir", mdir, envir = pkg_cache)
    return(mdir)
  } else {
    return(pkg_cache$mdir)
  }
}


identify_extract <- function(force = FALSE) {
  if (is.null(pkg_cache$extract_path) || force) {
    # Save docker working directory
    mdir <- get_ors_dir()

    # Read extract file location from compose file
    compose <- yaml::yaml.load_file(paste0(mdir, "/docker/docker-compose.yml"))
    extract_path <- compose$services$`ors-app`$build$args$OSM_FILE

    # Check if build argument is set
    if (is.null(extract_path)) {
      change_extract <- compose$services$`ors-app`$volumes[6]

      # If not, check if change volume is set
      if (is.na(change_extract)) {
        osm_file_occurences <- dir("docker/data") %>%
          grepl(".pbf|.osm.gz|.osm.zip|.osm", .)

        # As a last resort, check if we can just pick it up from the data folder
        if (sum(osm_file_occurences) == 1) {
          extract_path <- dir(
            paste0(mdir, "/docker/data")
          )[osm_file_occurences]
        } else {
          cli::cli_abort(
            paste(
              "Cannot identify current extract file. Pass it explicitly."
            )
          )
        }
      }
    }
    # Convert relative to absolute path
    extract_path <- basename(extract_path) %>%
      paste(mdir, "/docker/data", ., sep = "/") %>%
      normalizePath(winslash = "/")

    assign("extract_path", extract_path, envir = pkg_cache)
    return(extract_path)
  } else {
    return(pkg_cache$extract_path)
  }
}


get_ors_port <- function() {
  if (is.null(pkg_cache$port)) {
    container_info <- get_container_info()
    port <- container_info$NetworkSettings$Ports[[1]][[1]]$HostPort[1]
    assign("port", port, envir = pkg_cache)
  } else {
    return(pkg_cache$port)
  }
}
