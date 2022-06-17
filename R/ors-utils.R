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


inspect_container <- function(id = NULL) {
  if (is.null(ors_cache$container_info)) {
    if (missing(id) || is.null(id)) {
      id <- getOption("ors_name", "ors-app")
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


#' Get active ORS profiles
#' @description Returns a list of active profiles from the cache or local host.
#' @param force If \code{TRUE}, function must query the local host. If
#' \code{FALSE}, profiles will be read from the cache if possible.
#'
#' @export

get_profiles <- function(force = TRUE) {
  if (is.null(ors_cache$profiles) || isTRUE(force)) {
    ors_ready(force = force, error = TRUE)
    
    url <- sprintf("http://localhost:%s/ors/v2/status", get_ors_port())

    status_res <- httr::content(
      httr::GET(url),
      as = "text",
      type = "application/json",
      encoding = "UTF-8"
    )
    ors_info <- jsonlite::fromJSON(status_res)

    profiles <- unname(sapply(ors_info$profiles, function(x) x$profiles))
    assign("profiles", profiles, envir = ors_cache)
    profiles
  } else {
    ors_cache$profiles
  }
}


#' Is ORS usable?
#' @description States whether the ORS service is set up and ready to use.
#' @param force If \code{TRUE}, function must query local host. If
#' \code{FALSE}, the status will be read from the cache if possible.
#' @param error If \code{TRUE}, gives out an error if the service is not ready.
#'
#' @export

ors_ready <- function(force = TRUE, error = FALSE) {
  if (is.null(ors_cache$ors_ready) || isFALSE(ors_cache$ors_ready) || force) {
    url <- get_ors_url()
    port <- get_ors_port()
    host <- get_ors_host()

    if (is_local(url)) {
      if (host == "localhost") host <- "127.0.0.1"

      in_use <- tryCatch(
        expr = {
          s <- httpuv::startServer(
            host = host,
            port = as.numeric(port),
            app = list(),
            quiet = TRUE
          )
          s$stop()
          FALSE
        },
        error = function(e) {
          TRUE
        }
      )

      if (!in_use) {
        if (isTRUE(error)) {
          cli::cli_abort(c(
            "OpenRouteService instance is not reachable",
            "{.path {url}} is not currently in use."
          ))
        } else return(in_use)
      }
    }

    ready <- tryCatch(
      expr = {
        url <- file.path(url, "ors/health")
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
    assign("ors_ready", ready, envir = ors_cache)
    ready
  } else {
    ors_cache$ors_ready
  }
}


get_ors_dir <- function(force = TRUE) {
  if (is.null(ors_cache$mdir) || isTRUE(force)) {
    container_info <- inspect_container()
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


identify_extract <- function(force = FALSE) {
  if (is.null(ors_cache$extract_path) || force) {
    # Save docker working directory
    mdir <- get_ors_dir()

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


get_ors_port <- function(force = FALSE) {
  if (is.null(ors_cache$port) || force == TRUE) {
    container_info <- inspect_container()
    port <- container_info$NetworkSettings$Ports[[1]][[1]]$HostPort[1][1]
    assign("port", port, envir = ors_cache)
    port
  } else {
    ors_cache$port
  }
}


get_ors_url <- function() {
  options_url <- getOption("ors_url")
  if (is.null(options_url)) {
    sprintf("http://localhost:%s/", get_ors_port())
  } else {
    options_url
  }
}


get_ors_host <- function() {
  url <- get_ors_url()
  if (is_local(url)) {
    regex_match(url, "([[:alnum:]\\.]+)(?:\\:[[:digit:]]+)")[[1]][2]
  } else NULL
}


#' Return ORS conditions
#' @description Return the error and warning messages that ORS returned in the
#' last \code{\link{ors_distances}} or \code{get_route_attributes} function calls.
#' @param last Number of error lists that should be returned. \code{last = 2L},
#' for example, returns errors from the last two function calls.
#'
#' @export

last_ors_conditions <- function(last = 1L) {
  conditions <- ors_cache$routing_conditions

  if (length(conditions)) {
    cli_abortifnot(is.numeric(last))
    cli_abortifnot(last <= length(conditions))

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
