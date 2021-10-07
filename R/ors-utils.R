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
    container_info <- auth_system(
      "docker container inspect ors-app",
      stdout = TRUE
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
        httr::content(as = "text", type = "application/json", encoding = "UTF-8") %>%
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
      strsplit("/") %>%
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
    compose <- yaml::yaml.load_file(file.path(mdir, "docker/docker-compose.yml"))
    extract_path <- compose$services$`ors-app`$build$args$OSM_FILE

    # Check if build argument is set
    if (is.null(extract_path)) {
      volume <- compose$services$`ors-app`$volumes[6]
      extract_path <- gsub("\\./", "", strsplit(volume, ":")[[1]][1])

      # If not, check if change volume is set
      if (is.null(extract_path) || is.na(extract_path)) {
        osm_file_occurences <- dir("docker/data") %>%
          grepl(".pbf|.osm.gz|.osm.zip|.osm", .)

        # As a last resort, check if we can just pick it up from the data folder
        if (sum(osm_file_occurences) == 1) {
          extract_path <- dir(file.path(mdir,
                                        "docker/data"))[osm_file_occurences]
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
      file.path(mdir, "docker/data", .) %>%
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


fill_empty_error_message <- function(code) {
  switch(
    as.character(code),
    `2000` = "Unable to parse JSON request.",
    `2001` = "Required parameter is missing.",
    `2002` = "Invalid parameter format.",
    `2003` = "Invalid parameter value.",
    `2004` = "Parameter value exceeds the maximum allowed limit.",
    `2006` = "Unable to parse the request to the export handler.",
    `2007` = "Unsupported export format.",
    `2008` = "Empty Element.",
    `2009` = "Route could not be found between locations.",
    `2099` = "Unknown internal error.",
    `6000` = "Unable to parse JSON request.",
    `6001` = "Required parameter is missing.",
    `6002` = "Invalid parameter format.",
    `6003` = "Invalid parameter value.",
    `6004` = "Parameter value exceeds the maximum allowed limit.",
    `6006` = "Unable to parse the request to the export handler.",
    `6007` = "Unsupported export format.",
    `6008` = "Empty Element.",
    `6099` = "Unknown internal error."
  )
}


get_error_tip <- function(code) {
  switch(
    as.character(code),
    `2099` = paste("This error code might indicate that some or all input",
                  "coordinates fall outside of the extract coverage."),
    `6099` = paste("This error code typically occurs with with walking or",
                  "cycling profiles and matrix queries. Try increasing",
                  "{.val maximum_visited_nodes} in the ORS configuration file.")
  )
}