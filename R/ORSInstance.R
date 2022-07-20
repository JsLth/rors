# Title     : Local OpenRouteService backend initialization and control panel
# Objective : Set up, configure and change a local backend of OpenRouteService
# Created by: Jonas Lieth
# Created on: 29.07.2021


#options(
#  cli.spinner = "line",
#  cli.spinner_unicode = "line",
#  cli.spinner_ascii = "line",
#  cli.progress_bar_style = "fillsquares",
#  cli.progress_bar_style_unicode = "fillsquares",
#  cli.progress_bar_style_ascii = "fillsquares"
#)


#' OpenRouteService backend control panel
#' @description R6 class that acts as a setup wizard and control panel for the
#' OpenRouteService backend service. The class facilitates the setup of the
#' Docker container and allows making changes to the setup from within R.
#' 
#' This R6 class is deprecated in favor of \code{\link[ORSRouting]{ors_instance}}.
#'
#' @details The purpose of this class is to facilitate the OpenRouteService
#' installation process. Alternatively, you can follow the official
#' instructions from the \href{https://giscience.github.io/openrouteservice/installation/Advanced-Docker-Setup.html}{OpenRouteService documentation}.
#'
#' The class has four sub classes. \code{ORSExtract} manages the
#' OpenRouteService extract and is able to download `.pbf` files from different
#' sources using the `osmextract` package. \code{ORSConfig} controls the
#' configuration file (`ors-config.json`) which is also used to set active
#' profiles. \code{ORSSetupSettings} can be used to make changes to the
#' Docker setup, e.g. to allocate RAM, assign extracts or change the local
#' server access. \code{ORSDockerInterface} provides a basic interface
#' to Docker commands and can be used to check the status of the image,
#' container and service. The \code{ORSSetupSettings} should be set up
#' after \code{ORSExtract} and \code{ORSConfig} as the Docker setup
#' needs information on the extract and the number of profiles to assign the
#' extract and estimate the required RAM to be allocated.
#'
#' An initial default can be set up using \code{$init_setup}. This method
#' should only be used for the initial container startup. Subsequent changes
#' to the setup need to be done using the subclasses.
#'
#' @family ORSSetup
#' 
#' @rdname ORSInstance-deprecated

ORSInstance <- R6::R6Class(
  classname = "ORSInstance",
  active = list(

    #' @field dir Path to the ORS main directory
    dir = function() ORSInstance$funs$dir(),

    #' @field extract ORSExtract environment. Refer to
    #' \code{\link{ORSExtract}}.
    extract = function(arg) ORSInstance$funs$extract(private, arg),

    #' @field config ORSConfig environment. Refer to
    #' \code{\link{ORSConfig}}.
    config = function(arg) ORSInstance$funs$config(private, arg),

    #' @field setup_settings ORSSetupSettings environment. Refer to
    #' \code{\link{ORSSetupSettings}}.
    setup_settings = function(arg) ORSInstance$funs$setup_settings(private, arg),

    #' @field docker ORSDockerInterface environment. Refer to
    #' \code{\link{ORSDockerInterface}}.
    docker = function(arg) ORSInstance$funs$docker(private, arg)
  ),
  public = list(

    #' @field active States if the R6 object is active or if it has been
    #' killed using \code{$remove}. If \code{FALSE}, the fields and methods
    #' of \code{ORSInstance} lose their functionality.
    active = NULL,

    #' @description
    #' Initialize \code{\link{ORSInstance}} as well as \code{\link{ORSExtract}},
    #' \code{\link{ORSConfig}}, \code{\link{ORSSetupSettings}} and
    #' \code{\link{ORSDockerInterface}}.
    #' @param dir Custom ORS directory. If not specified, the directory will be
    #' downloaded to the system's home directory.
    #' @details It is recommended to pass a directory that is stable. If the
    #' Docker container is set up and the source directory is removed, a number
    #' of problems can emerge ranging from broken ORSRouting functions to the
    #' necessity to force-delete the container.
    initialize = function(dir = "~") {
      .Deprecated("ors_instance", package = "ORSRouting", old = "ORSInstance")
      
      if (!docker_installed()) {
        cli::cli_abort("Docker does not seem to be installed.")
      }

      if (is.linux() && !grant_docker_privileges(run = FALSE)) {
        cli::cli_abort(paste("To use {.cls ORSInstance}, Docker needs to be",
                             "accessible as a non-root user. Refer to the",
                             "function {.fn grant_docker_privileges}"))
      }

      dir <- private$.clone_ors_repo(dir)
      assign("mdir", dir, envir = ors_cache)

      self$active <- TRUE
      self$docker
      self$extract
      self$setup_settings
      self$config
      
      invisible(self)
    },

    #' @description Changes the necessary settings and configurations for the
    #' first startup, builds the image and starts the container. This function
    #' should only be used when starting the service for the first time. Changes
    #' after that should preferably be made manually.
    #' @param profiles Character vector. Modes of transport for which graphs
    #' should be build. Passed to \code{\link{ORSConfig}}.
    #' @param extract Path to an OSM extract that is then passed to
    #' \code{\link{ORSSetupSettings}}. Can also be any value that can be
    #' passed to \code{$extract$get_extract}. Defaults to `$extract$path`.
    #' This means that if you already set an extract using
    #' \code{\link{ORSExtract}}, you do not need to specify this argument. If
    #' you did not set an extract using \code{ORSExtract}, you can pass a path
    #' to a local extract here.
    #' @param provider Character vector of OSM extract provider(s) that should
    #' be searched for extracts.
    #' @param init_memory Initial memory to be allocated to the docker
    #' container. Passed to \code{\link{ORSSetupSettings}}.
    #' @param max_memory Maximum memory to be allocated to the docker
    #' container. The container will start with the initial memory and increases
    #' the memory usage up to the maximum memory if necessary.
    #' Passed to \code{\link{ORSSetupSettings}}.
    #' @param wait Logical. If `TRUE`, the function will not stop running after
    #' the container is being started and will give out a notification as soon
    #' as the service is ready. If `FALSE`, the function will start the
    #' container and then stop. To check the server status, you can then call
    #' `$service_ready` from the class \code{\link{ORSDockerInterface}}. Passed
    #' to \code{\link{ORSDockerInterface}}.
    #' @param verbose Logical. If \code{TRUE}, prints Docker logs for container
    #' setup.
    #' @param run Locial. If `TRUE`, returns `TRUE` if the initial setup is done
    #' and runs the setup if not. If `FALSE`, only returns logicals to check
    #' whether the initial setup is done or not.

    initial_setup = function(profiles = NULL,
                             extract = NULL,
                             provider = NULL,
                             init_memory = NULL,
                             max_memory = NULL,
                             wait = TRUE,
                             verbose = TRUE,
                             run = TRUE) {
      ORSInstance$funs$initial_setup(self, profiles, extract, provider, init_memory, max_memory, wait, verbose, run)
    },
    
    #' @description Take down all Docker containers and images and wipe the
    #' OpenRouteService main directory.
    #' @param ignore_image Specifies if the ORS image should be removed.
    #' Removing the image may have implications for other ORS instances or
    #' simply fail if a running container is using the same image.
    #' @details This function removes ORS entirely. It should only be used as a
    #' means to "uninstall" OpenRouteService.
    remove = function(ignore_image = TRUE) ORSInstance$funs$remove(self, ignore_image)

  ),
  private = list(
    .subclasses = list(),
    .get_subclass = function(env) ORSInstance$funs$get_subclass(self, private, env),
    .clone_ors_repo = function(dir) ORSInstance$funs$clone_ors_repo(dir)
  ),
  cloneable = FALSE
)


ORSInstance$funs <- new.env()

# Public methods --------------------------------------------------------------

ORSInstance$funs$dir <- function() {
  ors_cache$mdir
}


ORSInstance$funs$extract <- function(private, arg) {
  if (!missing(arg) && is.character(arg)) {
    refresh <- identical(arg, "refresh")
  } else {
    refresh <- FALSE
  }

  if (is.null(private$.subclasses$ORSExtract) || isTRUE(refresh)) {
    private$.get_subclass(ORSExtract)
  }
  return(private$.subclasses$ORSExtract)
}


ORSInstance$funs$config <- function(private, arg) {
  if (!missing(arg) && is.character(arg)) {
    refresh <- identical(arg, "refresh")
  } else {
    refresh <- FALSE
  }

  if (is.null(private$.subclasses$ORSConfig) || isTRUE(refresh)) {
    private$.get_subclass(ORSConfig)
  }
  return(private$.subclasses$ORSConfig)
}


ORSInstance$funs$setup_settings <- function(private, arg) {
  if (!missing(arg) && is.character(arg)) {
    refresh <- identical(arg, "refresh")
  } else {
    refresh <- FALSE
  }

  if (is.null(private$.subclasses$ORSSetupSettings) || isTRUE(refresh)) {
    private$.get_subclass(ORSSetupSettings)
  }
  return(private$.subclasses$ORSSetupSettings)
}


ORSInstance$funs$docker <- function(private, arg) {
  if (!missing(arg) && is.character(arg)) {
    refresh <- identical(arg, "refresh")
  } else {
    refresh <- FALSE
  }

  if(is.null(private$.subclasses$ORSDockerInterface) || isTRUE(refresh)) {
    private$.get_subclass(ORSDockerInterface)
  }
  return(private$.subclasses$ORSDockerInterface)
}


ORSInstance$funs$initial_setup <- function(self,
                                           profiles,
                                           extract,
                                           provider,
                                           init_memory,
                                           max_memory,
                                           wait,
                                           verbose,
                                           run) {
  if (isFALSE(self$active)) {
    cli::cli_warn("{.cls ORSInstance} is not active.")
    return(invisible())
  }

  if (self$docker$service_ready == "TRUE" ||
      (dir.exists(file.path(self$dir, "docker/graphs")) &&
       is.null(self$docker$error_log))) {
    return(TRUE)
  } else if (!run) {
    return(FALSE)
  }

  # Initialize extract --------------------------------------------------
  if (!is.null(extract)) {
    if (file.exists(extract)) {
      self$extract$set_extract(extract)
    } else {
      self$extract$get_extract(extract, provider = provider)
    }
  } else if (is.null(self$extract$path)) {
    cli::cli_abort("No extract set. Please pass one or set it using $extract.")
  }

  # Set up config ---------------------------------------------------
  if (!is.null(profiles)) {
    self$config$ors_config$ors$services$routing$profiles$active <- as.list(profiles)
    self$config$save_config()
  }

  # Set up Docker -------------------------------------------------------
  self$setup_settings$graph_building <- "build"
  self$setup_settings$allocate_memory(init_memory, max_memory)
  self$setup_settings$save_compose()

  # Start the service ---------------------------------------------------
  self$docker$container_up(wait, verbose)

  # Upddate ORSConfig
  self$config <- "refresh"

  invisible(TRUE)
}


ORSInstance$funs$remove <- function(self, ignore_image) {
  if (isFALSE(self$active)) {
    cli::cli_warn("{.cls ORSInstance} is not active.")
    return(invisible())
  }

  if (self$docker$container_built) self$docker$container_down()
  if (isFALSE(ignore_image)) {
    if (self$docker$image_exists) self$docker$rm_image()
  }

  if (dir.exists(self$dir)) {
    cli::cli_progress_step("Removing main directory...",
                           msg_done = "Removed main directory.",
                           msg_failed = "Cannot remove main directory.")
    unlink(self$dir, recursive = TRUE)
    cli::cli_progress_done()
  }
  
  cli::cli_progress_step("Terminating R6 class...",
                         msg_done = "Terminated R6 class.",
                         msg_failed = "Cannot remove R6 class object.")

  self$active <- FALSE
  suppressWarnings({
    self$extract <- "refresh"
    self$config <- "refresh"
    self$setup_settings <- "refresh"
    self$docker <- "refresh"
  })
}


# Private methods -------------------------------------------------------------

ORSInstance$funs$get_subclass <- function(self, private, env) {
  env_name <- deparse(substitute(env, env = parent.frame()))
  if (isTRUE(self$active)) {
    private$.subclasses[[env_name]] <- env$new()
  } else {
    cli::cli_warn("{.cls ORSInstance} is not active.")
    private$.subclasses[[env_name]] <- NULL
  }
}


ORSInstance$funs$clone_ors_repo <- function(dir) {
  download_url <- "https://github.com/GIScience/openrouteservice/archive/refs/heads/master.zip"

  cli_abortifnot(dir.exists(dir))

  dir <- normalizePath(dir, winslash = "/")
  basedir <- file.path(dir, "openrouteservice-master")

  if (!dir.exists(basedir)) {
    zip_file <- file.path(dir, "openrouteservice.zip")
    
    cli::cli_progress_step(msg = "Downloading service backend from GitHub repository...",
                           msg_done = "Successfully downloaded the service backend.",
                           msg_failed = "Failed to download the service backend.",
                           spinner = interactive())
    proc <- callr::r_bg(function(url, zip) {
      download.file(url, destfile = zip, quiet = TRUE)
    }, args = list(download_url, zip_file))
    while(proc$is_alive()) cli::cli_progress_update()
    cli::cli_progress_done()
    
    cli::cli_progress_step(msg = "Extracting files...",
                           msg_done = "Extracted files.",
                           msg_failed = "Could not extract files.",
                           spinner = interactive())
    
    proc <- callr::r_bg(function(zip, dir) {
      unzip(zip, exdir = dir)
    }, args = list(zip_file, dir))
    while(proc$is_alive()) cli::cli_progress_update()
    cli::cli_progress_done()
    
    cli::cli_progress_step(msg = "Removing zip file...",
                           msg_done = "Removed zip file.",
                           msg_failed = "Could not remove zip file.",
                           spinner = interactive())

    proc <- callr::r_bg(function(zip) {
      file.remove(zip)
    }, args = list(zip_file))
    while(proc$is_alive()) cli::cli_progress_update()
    cli::cli_progress_done()
  }
  
  basedir
}
