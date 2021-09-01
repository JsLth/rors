# Title     : OSM extract control panel
# Objective : Download, set or manage OSM extracts
# Created by: Jonas Lieth
# Created on: 17.08.2021



#' OpenRouteService OSM extract control panel
#' @description R6 class to download, set or manage an OpenStreetMap extract
#'
#' @details Note that the coverage of the OSM extract should include all
#' necessary places that need to be processed. If a location is not covered
#' by the extract, the service will respond with an error. If necessary, the
#' OSM extract can be changed later by running `$assign_data(build = change)`
#' in \code{\link{ORSSetupSettings}}.
#'
#' @importFrom magrittr %>%

ORSExtract <- R6::R6Class(
  classname = "ORSExtract",
  public = list(

    #' @field path Relative path to the extract.
    path = NULL,

    #' @field place Name or coordinates of the place that is passed for the
    #' extract download.
    place = NULL,

    #' @field level Hierarchical level of the extraxt to be matched.
    level = NULL,

    #' @field size Size of the extract file in megabytes
    size = NULL,

    #' @field provider Provider of the extract size.
    provider = NULL,

    #' @description Initializes \code{\link{ORSExtract}} and looks for an
    #' existing extract in data path.
    initialize = function() {
      osm_file_occurences <- dir("docker/data") %>%
        grepl(".pbf|.osm.gz|.osm.zip|.osm", .)
      if (sum(osm_file_occurences) == 1) {
        path <- paste0("docker/data/", dir("docker/data")[osm_file_occurences])
        self$path <- path
        self$size <- file.info(path)$size * 0.000001
      } else if (sum(osm_file_occurences) > 1) {
        cli::cli_alert_warning(
          paste(
            "Multiple OSM files found in the data directory.",
            "Please set an extract manually."
          )
        )
      }
    },

    #' @description Downloads an OSM extract.
    #' @param place Character scalar, sf or sfc object or length-2 numeric
    #' vector to be passed to \code{\link[osmextract]{oe_match}}. Represents
    #' a place that falls inside the coverage of defined extract regions of
    #' the providers listed in \code{\link[osmextract]{oe_providers}}. The
    #' geographic scale can be adjusted by changing the parameter `level`. For
    #' details, refer to \code{\link[osmextract]{oe_match}}.
    #' @param ... Passed to \code{\link[osmextract]{oe_match}}.
    #' @details The extract is downloaded directly to docker/data. This will
    #' also be the directory that is passed to the
    #' \code{\link{ORSSetupSettings}} to process the extract. This directory is
    #' not mutable because Docker expects a relative path to its main directory.
    get_extract = function(place, ...) {
      download_path <- "docker/data"
      ok <- TRUE
      i <- 0
      providers <- suppressMessages({
        osmextract::oe_providers()$available_providers
      })
      cli::cli_alert_info("Trying different extract providers...")
      while (ok && i < length(providers)) {
        i <- i + 1
        place_match <- osmextract::oe_match(
          place,
          provider = providers[i],
          quiet = TRUE,
          ...
        )
        file_name <- strsplit(place_match$url, "/") %>% .[[1]] %>% tail(1)
        cli::cli_alert_info(
          paste(
            "The extract {.file {file_name}} is",
            "{.field {round(place_match$file_size / 1024 / 1024)}} MB in size",
            "and will be downloaded from {.field {providers[i]}}."
          )
        )
        if (providers[i] == "bbbike") {
          cli::cli_alert_warning(
            paste(
              "bbbike extracts are known to cause issues with memory",
              "allocation. Use with caution."
            )
          )
        }
        input <- tolower(
          readline(
            "Should a different provider be tried? (Yes/No/Cancel)"
          )
        )
        if (!input %in% c("yes", "no")) {
          cli::cli_alert_danger("Function cancelled.")
          invokeRestart("abort")
        }
        ok <- input == "yes"
      }
      if (ok) {
        cli::cli_alert_warning(
          paste(
            "All providers have been searched.",
            "Please download the extract manually."
          )
        )
        invokeRestart("abort")
      }
      file_occurences <- grepl(file_name, dir(download_path))
      if (sum(file_occurences) == 1) {
        cli::cli_alert_info(
          paste(
            "The extract already exists in {.file /docker/data}.",
            "Download will be skipped."
          )
        )
        path <- paste(
          download_path,
          dir(download_path)[file_occurences],
          sep = "/"
        )
        cli::cli_text("Download path: {.file {path}}")
      } else {
        private$.rm_old_extracts()
        path <- paste0(download_path, "/", providers[i], "_", file_name)
        cli::cli_progress_step(
          "Downloading the OSM extract...",
          msg_done = paste(
            "The extract was successfully downloaded to the following path:",
            "{.file {path}}"
          ),
          msg_failed = "Extract could not be downloaded."
        )
        osmextract::oe_download(
          place_match$url,
          provider = providers[i],
          download_directory = normalizePath(download_path, winslash = "/"),
          quiet = TRUE
        )
        cli::cli_progress_done()
      }
      size <- file.info(path)$size / 1024 / 1024
      if (size >= 6000) {
        cli::cli_alert_warning(
          paste(
            "The OSM extract is very large.",
            "Make sure that you have enough working memory available."
          )
        )
      }
      self$path <- path
      self$place <- place
      self$size <- round(size, 2)
      self$provider <- providers[i]
    },

    #' @description Moves a given OSM extract to the ORS data directory
    #' @param extract_path Character scalar. Path to an OSM extract formatted
    #' as `.pbf`, `.osm`, `.osm.gz`
    #' or `.osm.zip`.
    set_extract = function(extract_path) {
      if (file.exists(extract_path)) {
        self$path <- relativePath(private$.move_extract(extract_path))
        self$size <- file.info(extract_path)$size * 0.000001
      } else {
        cli::cli_abort("{.file {extract_path}} does not exist.")
      }
    }
  ),
  private = list(
    .move_extract = function(extract_path) {
      # Derive file name from file path
      file_name <- extract_path %>%
      strsplit("/") %>%
      unlist() %>%
      tail(1)

      # Move extract to ./docker/data
      file.copy(extract_path, "docker/data")
      paste("docker/data", file_name, sep = "/")
    },
    .rm_old_extracts = function() {
      path <- normalizePath("docker/data", winslash = "/")
      extract_occurences <- dir(path) %>%
        grepl(".pbf|.osm.gz|.osm.zip|.osm", .)
      if (sum(extract_occurences) > 0) {
        cli::cli_alert_info("Removing old extracts...")
        for (extract in dir(path)[extract_occurences]) {
          file.remove(paste0("docker/data/", extract))
        }
      }
    }
  ),
  cloneable = FALSE
)
