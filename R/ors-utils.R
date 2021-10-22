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
    container_info <- system2(command = "docker",
                              args = "container inspect ors-app",
                              stdout = TRUE) %>%
      paste0(collapse = "\n") %>%
      jsonlite::fromJSON()

    if (length(container_info) == 0) {
      cli::cli_abort("Cannot access container: ors-app")
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
#'
#' @export

ors_ready <- function(force = TRUE, error = FALSE) {
  if (is.null(pkg_cache$ors_ready) || force) {
    port <- get_ors_port()
    ready <- tryCatch(
      httr::GET(
        sprintf("http://localhost:%s/ors/health", port)
      ) %>%
        httr::content(as = "text", type = "application/json", encoding = "UTF-8") %>%
        jsonlite::fromJSON() %>%
        .$status %>%
        identical("ready"),
      error = function(e) {
        ifelse(error,
               cli::cli_abort("ORS service is not reachable."),
               FALSE)
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
    container_info <- get_container_info()
    mdir <- container_info$Config$Labels$com.docker.compose.project.working_dir
    mdir <- normalizePath(mdir, winslash = "/") %>%
      strsplit("/") %>%
      unlist() %>%
      head(-1) %>%
      paste0(collapse = "/")

    if (!dir.exists(mdir)) {
      cli::cli_abort("The current ORS container directory does not exist")
    }

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


get_ors_url <- function() {
  options_url <- getOption("ors_url")
  if (is.null(options_url)) {
    sprintf("http://localhost:%s/", get_ors_port())
  } else {
    options_url
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


fill_steepness <- function(code) {
  switch(
    as.character(code),
    `-5` = ">16%",
    `-4` = "12-15%",
    `-3` = "7-11%",
    `-2` = "4-6%",
    `-1` = "1-3%",
    `0`  = "0%",
    `1`  = "1-3%",
    `2`  = "4-6%",
    `3`  = "7-11%",
    `4`  = "12-15%",
    `5`  = ">16%"
  )
}


fill_surface <- function(code) {
  switch(
    as.character(code),
    `0`  = "Unknown",
    `1`  = "Paved",
    `2`  = "Unpaved",
    `3`  = "Asphalt",
    `4`  = "Concrete",
    `5`  = "Cobblestone",
    `6`  = "Metal",
    `7`  = "Wood",
    `8`  = "Compacted Gravel",
    `9`  = "Fine Gravel",
    `10` = "Gravel",
    `11` = "Dirt",
    `12` = "Ground",
    `13` = "Ice",
    `14` = "Paving Stones",
    `15` = "Sand",
    `16` = "Woodchips",
    `17` = "Grass",
    `18` = "Grass Paver"
  )
}


fill_waycategory <- function(code) {
  switch(
    as.character(code),
    `0`   = "No category",
    `1`   = "Highway",
    `2`   = "Steps",
    `4`   = "Unpaved Road",
    `8`   = "Ferry",
    `16`  = "Track",
    `32`  = "Tunnel",
    `64`  = "Paved road",
    `128` = "Ford"
  )
}


fill_waytypes <- function(code) {
  switch(
    as.character(code),
    `0`  = "Unknown",
    `1`  = "State Road",
    `2`  = "Road",
    `3`  = "Street",
    `4`  = "Path",
    `5`  = "Track",
    `6`  = "Cycleway",
    `7`  = "Footway",
    `8`  = "Steps",
    `9`  = "Ferry",
    `10` = "Construction"
  )
}


fill_traildifficulty <- function(code, profile) {
  if (identical(base_profile(profile), "cycling")) {
    code <- code * -1
  }

  switch(
    as.character(code),
    `-7` = "mtb:scale=6",
    `-6` = "mtb:scale=5",
    `-5` = "mtb:scale=4",
    `-4` = "mtb:scale=3",
    `-3` = "mtb:scale=2",
    `-2` = "mtb:scale=1",
    `-1` = "mtb:scale=0",
    `0`  = "No Tag",
    `1`  = "sac_scale=hiking",
    `2`  = "sac_scale=mountain_hiking",
    `3`  = "sac_scale=demanding_mountain_hiking",
    `4`  = "sac_scale=alpine_hiking",
    `5`  = "sac_scale=demanding_alpine_hiking",
    `6`  = "sac_scale=difficult_alpine_hiking"
  )
}


fill_roadaccessrestrictions <- function(code) {
  switch(
    as.character(code),
    `0`  = "None",
    `1`  = "No",
    `2`  = "Customers",
    `4`  = "Destination",
    `8`  = "Delivery",
    `16` = "Private",
    `32` = "Permissive"
  )
}


fill_extra_info <- function(values, info_type, profile) {
  fill_fun_name <- paste("fill", info_type, sep = "_")
  if (exists(fill_fun_name)) {
    fill_fun <- match.fun(fill_fun_name)
    if (identical(fill_fun_name, "fill_traildifficulty")) {
      values <- sapply(values, fill_fun, profile)
    } else {
      values <- sapply(values, fill_fun)
    }
  }
  values
}


handle_ors_conditions <- function(res, abort_on_error = FALSE, warn_on_warning = FALSE) {
  if (!is.null(res$error)) {
    message <- res$error$message
    code <- res$error$code
    if (is.null(res$error$message)) {
      message <- fill_empty_error_message(res$error$code)
    }
    error <- paste0("Error code ", code, ": ", message)
    if (abort_on_error) {
      cli::cli_abort(c("ORS encountered the following exception:", error))
    } else {
      attributes(error) <- list(error = TRUE)
      return(error)
    }
  } else {
    format <- res$metadata$query$format
    if (identical(format, "geojson")) {
      warnings_geojson <- res$features$properties$warnings
      warnings_json <- res$routes$warnings
      message <- NULL
      code <- NULL
      if (!is.null(warnings_geojson)) {
        message <- purrr::map(warnings_geojson, ~.$message)
        code <- purrr::map(warnings_geojson, ~.$code)
      } else if (!is.null(warnings_json)) {
        message <- purrr::map(warnings_json, ~.$message)
        code <- purrr::map(warnings_json, ~.$code)
      }

      if (length(code) && length(message)) {
        warnings <- purrr::map(seq(1, length(code)), function(w) {
          paste0("Warning code ", code[w], ": ", message[w])
        })

        if (warn_on_warning) {
          w_vec <- cli::cli_vec(warnings,
                                style = list(vec_sep = "\n", vec_last = "\n"))
          cli::cli_warn(c("ORS returned {length(w_vec)} warning{?s}:", "{w_vec}"))
        } else {
          attributes(warning) <- list(error = FALSE)
          return(warnings)
        }
      }
    }
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

  cli_abortifnot(is.numeric(last))
  cli_abortifnot(last <= length(conditions))

  time <- names(conditions)

  cond_df <- lapply(conditions, function(x) {
    na.omit(data.frame(conditions = x))
  })
  names(cond_df) <- time

  end <- length(names(cond_df))
  start <- length(names(cond_df)) + 1 - last
  selected_conditions <- cond_df[seq(start, end)]

  class(selected_conditions) <- "ors_condition"
  selected_conditions
}


#' Print ORS conditions
#' @description Print the output of \code{last_ors_conditions}.
#' @param x Object of type \code{ors_condition}
#' @param ... Ignored.
#'
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
