# Title     : OSM extract control panel
# Objective : Download, set or manage OSM extracts
# Created by: Jonas Lieth
# Created on: 17.08.2021



#' OpenRouteService OSM extract control panel
#' @description R6 class to download, set or manage an OpenStreetMap extract.
#' \strong{This class is initialized from within \code{\link{ORSInstance}}}.
#'
#' @details Note that the coverage of the OSM extract should include all
#' necessary places that need to be processed. If a location is not covered
#' by the extract, the service will respond with an error. If necessary, the
#' OSM extract can be changed later by running `$assign_data(build = change)`
#' in \code{\link{ORSSetupSettings}}.
#'
#' @family ORSSetup

ORSExtract <- R6::R6Class(
  classname = "ORSExtract",
  inherit = ORSInstance,
  public = list(

    #' @field path Relative path to the extract.
    path = NULL,

    #' @field size Size of the extract file in megabytes
    size = NULL,

    #' @description Initializes \code{\link{ORSExtract}} and looks for an
    #' existing extract in data path.
    initialize = function() {
      private$.set_current_extract()
      self$active <- TRUE
      invisible(self)
    },

    #' @description Downloads an OSM extract.
    #' @param place Character scalar, \code{sf} or \code{sfc} object or length-2
    #' numeric vector to be passed to \code{\link[osmextract]{oe_match}}.
    #' Represents a place that falls inside the coverage of defined extract
    #' regions of the providers listed in \code{\link[osmextract]{oe_providers}}.
    #' The geographic scale can be adjusted by changing the parameter `level`.
    #' For details, refer to \code{\link[osmextract]{oe_match}}.
    #' @param provider Character vector of OSM extract provider(s) that should
    #' be searched for extracts.
    #' @param ... Passed to \code{\link[osmextract]{oe_match}}. This can
    #' include anything except \code{provider} and \code{quiet}.
    #' @details The extract is downloaded directly to \code{docker/data}. This 
    #' will also be the directory that is passed to the
    #' \code{\link{ORSSetupSettings}} to process the extract. This directory is
    #' not mutable because Docker expects a relative path to its main directory.

    get_extract = function(place, provider = NULL, ...) ORSExtract$funs$get_extract(self, private, place, provider, ...),

    #' @description Moves a given OSM extract to the ORS data directory
    #' @param extract_path Character scalar. Path to an OSM extract formatted
    #' as `.pbf`, `.osm`, `.osm.gz` or `.osm.zip`.

    set_extract = function(extract_path) ORSConfig$funs$set_extract(self, private, extract_path)
  ),

  private = list(
    .move_extract = function(extract_path) ORSConfig$funs$move_extract(self, extract_path),
    .identify_extract_files = function() ORSConfig$funs$identify_extract_files(self),
    .set_current_extract = function() ORSConfig$funs$set_current_extract(self, private),
    .rm_old_extracts = function() ORSConfig$funs$rm_old_extracts(self, private)
  ),
  cloneable = FALSE
)


ORSExtract$funs <- new.env()

# Public methods --------------------------------------------------------------

ORSExtract$funs$get_extract <- function(self, private, place, provider = NULL, ...) {
  data_dir <- file.path(self$dir, "docker/data")
  ok <- TRUE
  i <- 0

  if (is.null(provider)) {
    providers <- suppressMessages({
      osmextract::oe_providers()$available_providers
    })
  } else {
    providers <- provider
  }

  if (!interactive() && length(providers) > 1) {
    cli::cli_abort(paste("In batch mode, explicitly pass",
                         "a single provider name."))
  }

  cli::cli_alert_info("Trying different extract providers...")

  # While there are providers left to try out, keep trying until
  # an extract provider is chosen
  while (ok && i < length(providers)) {
    i <- i + 1
    place_match <- osmextract::oe_match(place = place,
                                        provider = providers[i],
                                        quiet = TRUE,
                                        ...)

    file_name <- basename(place_match$url)
    file_size <- round(place_match$file_size / 1024 / 1024)

    cli::cli_alert_info(paste("The extract {.file {file_name}} is",
                              "{.field {file_size}} MB in size",
                              "and will be downloaded from",
                              "{.field {providers[i]}}."))

    if (providers[i] == "bbbike") {
      cli::cli_alert_warning(paste("bbbike extracts are known to cause issues",
                                   "with memory allocation. Use with caution."))
    }

    if (length(providers) > 1) {
      input <- tolower(readline(paste("Should a different provider",
                                      "be tried? (Yes/No/Cancel)")))
    } else {
      input <- "no"
    }

    # If neither yes or no is given as input, cancel the function
    if (!input %in% c("yes", "no")) {
      cli::cli_alert_danger("Function cancelled.")
      invokeRestart("abort")
    }
    ok <- input == "yes"
  }

  # If the while loop exits and the last answer given is yes, exit
  if (ok) {
    cli::cli_alert_warning(paste("All providers have been searched.",
                                 "Please download the extract manually."))
    invokeRestart("abort")
  }

  # If a file with the same name already exists, skip the download
  file_occurences <- grepl(file_name, dir(data_dir))
  if (sum(file_occurences) == 1) {
    cli::cli_alert_info(paste("The extract already exists in",
                              "{.file /docker/data}.",
                              "Download will be skipped."))

    path <- paste(data_dir,
                  dir(data_dir)[file_occurences],
                  sep = "/")

    cli::cli_text("Download path: {.file {path}}")

    # If no file or several files with the same name exist, remove all
    # older files and download a new one
  } else {
    private$.rm_old_extracts()
    path <- file.path(data_dir, paste0(providers[i], "_", file_name))

    cli::cli_progress_step("Downloading the OSM extract...",
                           msg_done = paste("The extract was successfully",
                                            "downloaded to the following path:",
                                             "{.file {path}}"),
                           msg_failed = "Extract could not be downloaded.")

    osmextract::oe_download(place_match$url,
                            provider = providers[i],
                            download_directory = data_dir,
                            quiet = TRUE)

    cli::cli_progress_done()
  }

  # If the size is over 6 GB in size, give out a warning
  size <- file.info(path)$size / 1024 / 1024
  if (size >= 6000) {
    cli::cli_alert_warning(paste("The OSM extract is very large.",
                                 "Make sure that you have enough",
                                 "working memory available."))
  }

  # Set the extract
  self$set_extract(path)
  invisible(path)
}



ORSConfig$funs$set_extract <- function(self, private, extract_path) {
  cli_abortifnot(file.exists(extract_path))

  self$path <- private$.move_extract(extract_path)
  self$size <- round(file.info(extract_path)$size * 0.000001, 2)

  assign("extract_path", self$path, envir = pkg_cache)
  return(self$path)
}


# Private methods -------------------------------------------------------------

ORSConfig$funs$move_extract <- function(self, extract_path) {
  data_dir <- file.path(self$dir, "docker/data")
  extract_path <- normalizePath(extract_path, winslash = "/")
  file_name <- basename(extract_path)
  new_path <- file.path(data_dir, file_name)

  if (!identical(extract_path, new_path)) {
    # Move extract to ./docker/data
    file.copy(extract_path, data_dir)
  }
  new_path
}


ORSConfig$funs$identify_extract_files <- function(self) {
  data_dir <- file.path(self$dir, "docker/data")
  osm_file_occurences <- grepl(pattern = ".pbf|.osm.gz|.osm.zip|.osm",
                               x = dir(data_dir))
}


ORSConfig$funs$set_current_extract <- function(self, private) {
  data_dir <- file.path(self$dir, "docker/data")
  extract_occurences <- private$.identify_extract_files

  # If exactly one file is an extract, set it
  if (sum(extract_occurences) == 1) {
    path <- file.path(data_dir, dir(data_dir)[extract_occurences])
    self$set_extract(path)

    # If more than one file is an extract, can't choose one.
  } else if (sum(extract_occurences) > 1) {
    cli::cli_alert_warning(paste("Multiple OSM files found in the data",
                                 "directory. Please set an extract manually."))
  }
}


ORSConfig$funs$rm_old_extracts <- function(self, private) {
  data_dir <- file.path(self$dir, "docker/data")
  extract_occurences <- private$.identify_extract_files

  if (sum(extract_occurences) > 0) {
    cli::cli_alert_info("Removing old extracts...")
    for (extract in dir(data_dir)[extract_occurences]) {
      file.remove(file.path(data_dir, extract))
    }
  }
}