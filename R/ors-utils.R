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


inspect_container <- function() {
  if (is.null(pkg_cache$container_info)) {
    name <- getOption("ors_name")
    
    cmd <- c("container", "inspect", name)
    
    suppressWarnings({
      container_info <- system2(command = "docker",
                                args = cmd,
                                stdout = TRUE,
                                stderr = FALSE) %>%
        paste0(collapse = "\n") %>%
        jsonlite::fromJSON()
    })

    if (length(container_info) == 0) {
      cli::cli_abort("Cannot access container: {.val {name}}")
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

get_profiles <- function(force = TRUE) {
  if (is.null(pkg_cache$profiles) || isTRUE(force)) {
    if (ors_ready()) {
      url <- sprintf("http://localhost:%s/ors/v2/status", get_ors_port())
    } else {
      cli::cli_abort("ORS service is not reachable.")
    }

    status_response <- httr::GET(url) %>%
      httr::content(as = "text", type = "application/json", encoding = "UTF-8")
    ors_info <- jsonlite::fromJSON(status_response)

    profiles <- unname(sapply(ors_info$profiles, function(x) x$profiles))
    assign("profiles", profiles, envir = pkg_cache)
    return(profiles)
  } else {
    return(pkg_cache$profiles)
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
  if (is.null(pkg_cache$ors_ready) || isFALSE(pkg_cache$ors_ready) || force) {
    ready <- tryCatch(
      {
        res <- httr::GET(sprintf("http://localhost:%s/ors/health",
                         get_ors_port())) %>%
          httr::content(as = "text",
                        type = "application/json",
                        encoding = "UTF-8") %>%
          jsonlite::fromJSON()
        stopifnot(r <- identical(res$status, "ready"))
        r
      },
      error = function(e) {
        if (isTRUE(error)) {
          cli::cli_abort("ORS service is not reachable.")
        } else FALSE
      }
    )
    assign("ors_ready", ready, envir = pkg_cache)
    return(ready)
  } else {
    return(pkg_cache$ors_ready)
  }
}


get_ors_dir <- function(force = TRUE) {
  if (is.null(pkg_cache$mdir) || isTRUE(force)) {
    container_info <- inspect_container()
    mdir <- container_info$Config$Labels$com.docker.compose.project.working_dir
    mdir <- normalizePath(mdir, winslash = "/") %>%
      strsplit("/") %>%
      unlist() %>%
      utils::head(-1) %>%
      paste0(collapse = "/")

    if (!dir.exists(mdir)) {
      cli::cli_abort("The current ORS container directory does not exist")
    }

    assign("mdir", mdir, envir = pkg_cache)
    mdir
  } else {
    pkg_cache$mdir
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
        data_dir <- file.path(mdir, "docker/data")
        osm_file_occurences <- dir(data_dir) %>%
          grepl(".pbf|.osm.gz|.osm.zip|.osm", .)

        # As a last resort, check if we can just pick it up from the data folder
        if (sum(osm_file_occurences) == 1) {
          extract_path <- dir(data_dir)[osm_file_occurences]
        } else {
          cli::cli_abort(paste("Cannot identify current extract file.",
                               "Pass it explicitly."))
        }
      }
    }

    # Convert relative to absolute path
    extract_path <- basename(extract_path) %>%
      file.path(mdir, "docker/data", .) %>%
      normalizePath(winslash = "/")

    assign("extract_path", extract_path, envir = pkg_cache)
    extract_path
  } else {
    pkg_cache$extract_path
  }
}


get_ors_port <- function(force = TRUE) {
  if (is.null(pkg_cache$port) || force == TRUE) {
    container_info <- inspect_container()
    port <- container_info$NetworkSettings$Ports[[1]][[1]]$HostPort[1][1]
    assign("port", port, envir = pkg_cache)
    port
  } else {
    pkg_cache$port
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


#' Print ORS errors
#' @description Return the error and warning messages that ORS returned in the
#' last \code{\link{get_route_lengths}} or \code{get_route_attributes} function calls.
#' @param last Number of error lists that should be returned. \code{last = 2L},
#' for example, returns errors from the last two function calls.
#'
#' @export

last_ors_conditions <- function(last = 1L) {
  conditions <- pkg_cache$routing_conditions

  if (length(conditions)) {
    cli_abortifnot(is.numeric(last))
    cli_abortifnot(last <= length(conditions))

    time <- names(conditions)

    cond_df <- lapply(conditions, function(x) {
      stats::na.omit(data.frame(conditions = x))
    })
    names(cond_df) <- time

    end <- length(names(cond_df))
    start <- length(names(cond_df)) + 1 - last
    selected_conditions <- cond_df[seq(start, end)]

    class(selected_conditions) <- "ors_condition"
    selected_conditions
  }
}


#' @export

print.ors_condition <- function(x, ...) {
  timestamps <- names(x)
  calls <- sapply(timestamps, function(time) {
    indices <- attr(x[[time]], "row.names")
    messages <- sapply(x[[time]]$conditions, function(row) {
      if (nchar(row) > 100) {
        paste0(strtrim(row, width = 100), "...")
      } else row
    })
    messages <- lapply(seq(1, length(messages)),
                       function(mi) paste0(indices[mi], " - ", messages[[mi]]))
    printed_time <- paste0("Function call from ", time, ":")
    paste(printed_time, paste0(messages, collapse = "\n"), sep = "\n")
  })
  cat(paste0(paste0(calls, collapse = "\n\n"), "\n"))
}
