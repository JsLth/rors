# Title     : Local OpenRouteService backend initialization and control panel
# Objective : Set up, configure and change a local backend of OpenRouteService
# Created by: Jonas Lieth
# Created on: 29.07.2021


options(
  cli.spinner = "line",
  cli.spinner_unicode = "line",
  cli.spinner_ascii = "line"
)


#' OpenRouteService backend control panel
#' @description R6 class that acts as a setup wizard and control panel for the
#' OpenRouteService backend service. The class facilitates the setup of the
#' Docker container and allows making changes to the setup from within R.
#'
#' @details The purpose of this class is to facilitate the OpenRouteService
#' installation process. Alternatively, you can follow the official
#' instructions from the \href{https://giscience.github.io/openrouteservice/installation/Advanced-Docker-Setup.html}{OpenRouteService documentation}.
#' The developer team recently extended the installation guide considerably.
#'
#' The class has four sub classes. \code{\link{ORSExtract}} manages the
#' OpenRouteService extract and is able to download `.pbf` files from different
#' sources using the `osmextract` package. \code{\link{ORSConfig}} controls the
#' configuration file (`ors-config.json`) which is also used to set active
#' profiles. \code{\link{ORSSetupSettings}} can be used to make changes to the
#' Docker setup, e.g. to allocate RAM, assign extracts or change the local
#' server access. \code{\link{ORSDockerInterface}} provides a basic interface
#' to Docker commands and can be used to check the status of the image,
#' container and service. \code{\link{ORSSetupSettings}} should be initialized
#' last as the Docker setup needs information on the extract and the number of
#' profiles to assign the extract and estimate the required RAM to be
#' allocated.
#'
#' If the setup keeps failing due to whatever reason, try resetting the docker
#' path of the main directory (as specified in `$dir`), or just delete the
#' directory and download it again to be safe.
#'
#' If the setup fails due to an OutOfMemoryError, first check if you allocated
#' enough memory. If it keeps failing, clear the available memory or restart
#' the system. OpenRouteService recommends allocating a little more than twice
#' the extract size. Make sure not to allocate more than your available memory.
#' If you allocate more than 80% of your free working memory, the function will
#' stop. For details refer to the
#' \href{https://giscience.github.io/openrouteservice/installation/System-Requirements.html}{system requirements of OpenRouteService}
#'
#' @seealso
#' \code{\link{ORSExtract}},
#' \code{\link{ORSConfig}},
#' \code{\link{ORSDockerInterface}},
#' \code{\link{ORSSSetupSettings}}
#'
#' @importFrom magrittr %>%
#'
#' @export

ORSInstance <- R6::R6Class(
  classname = "ORSInstance",
  active = list(

    #' @field dir Path to the ORS main directory
    dir = function() {
      pkg_cache$mdir
    },

    #' @field extract ORSExtract environment. Refer to
    #' \code{\link{ORSExtract}}.
    extract = function(arg) {
      if (is.null(private$.subclasses$ORSExtract)) {
        private$.get_subclass(ORSExtract)
      }
      return(private$.subclasses$ORSExtract)
    },

    #' @field config ORSConfig environment. Refer to
    #' \code{\link{ORSConfig}}.
    config = function(arg) {
      if (!missing(arg) && is.character(arg)) {
        refresh <- identical(arg, "refresh")
      } else {
        refresh <- FALSE
      }

      if (
        is.null(private$.subclasses$ORSConfig) ||
        isTRUE(refresh)
      ) {
        private$.get_subclass(ORSConfig)
      }
      return(private$.subclasses$ORSConfig)
    },

    #' @field setup_settings ORSSetupSettings environment. Refer to
    #' \code{\link{ORSSetupSettings}}.
    setup_settings = function(arg) {
      if (!missing(arg) && is.character(arg)) {
        refresh <- identical(arg, "refresh")
      } else {
        refresh <- FALSE
      }

      if (
        is.null(private$.subclasses$ORSSetupSettings) || isTRUE(refresh)) {
        private$.get_subclass(ORSSetupSettings)
      }
      return(private$.subclasses$ORSSetupSettings)
    },

    #' @field docker ORSDockerInterface environment. Refer to
    #' \code{\link{ORSDockerInterface}}.
    docker = function(arg) {
      if(is.null(private$.subclasses$ORSDockerInterface)) {
        private$.get_subclass(ORSDockerInterface)
      }
      return(private$.subclasses$ORSDockerInterface)
    }
  ),
  public = list(

    #' @description
    #' Initialize \code{\link{ORSInstance}} as well as \code{\link{ORSExtract}},
    #' \code{\link{ORSConfig}}, \code{\link{ORSSetupSettings}} and
    #' \code{\link{ORSDockerInterface}}.
    #' @param dir Custom ORS directory. If not specified, the directory will be
    #' downloaded to the package directory.
    initialize = function(dir = NULL) {
      if (!is.null(dir)) {
        private$.clone_ors_repo(dir)
      } else {
        dir <- private$.clone_ors_repo()
      }
      assign("mdir", dir, envir = pkg_cache)

      self$docker
      self$extract
      self$setup_settings
      self$config
    },

    #' @description Changes the necessary settings and configurations for the
    #' first startup, builds the image and starts the container. This function
    #' should only be used when starting the service for the first time. Changes
    #' after that should preferably be made manually.
    #' @param profiles Character vector. Modes of transport for which graphs
    #' should be build. Passed to \code{\link{ORSConfig}}.
    #' @param extract_path Path to an OSM extract that is then passed to
    #' \code{\link{ORSSetupSettings}}. Defaults to `$extract$path`. This means
    #' that if you already set an extract using \code{\link{ORSExtract}}, you
    #' do not need to specify this argument. If you did not set an extract
    #' using ORSExtract, you can pass a path to a local extract here.
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
    #' @param run Locial. If `TRUE`, returns `TRUE` if the initial setup is done
    #' and runs the setup if not. If `FALSE`, only returns logicals to check
    #' whether the initial setup is done or not.

    initial_setup = function(
      profiles = "car",
      extract_path = self$extract$path,
      init_memory = NULL,
      max_memory = NULL,
      wait = TRUE,
      run = TRUE) {
        if (
          (self$docker$service_ready == "TRUE" ||
          dir.exists(file.path(self$dir, "docker/graphs"))) &&
          is.null(self$docker$error_log)
        ) {
          return(TRUE)
        } else if (!run) {
          return(FALSE)
        }

        # Initialize extract --------------------------------------------------
        if (!missing(extract_path)) {
          self$extract$set_extract(extract_path)
        } else {
          if (is.null(extract_path)) {
            cli::cli_abort(
              paste(
                "Either pass an extract path or set an extract using",
                "{.cls ORSExtract}"
              )
            )
          }
        }

        # Set up config ---------------------------------------------------
        self$
          config$
          ors_config$
          ors$
          services$
          routing$
          profiles$
          active <- as.list(profiles)
        self$config$save_config()

        # Set up Docker -------------------------------------------------------
        self$setup_settings$graph_building <- "build"
        self$setup_settings$allocate_memory(init_memory, max_memory)
        self$setup_settings$save_compose()

        # Start the service ---------------------------------------------------
        self$docker$image_up(wait)

        # Turn off graph building
        self$setup_settings$graph_building <- NA

        # Upddate ORSConfig
        self$config <- "refresh"
    }
  ),
  private = list(
    .subclasses = list(),

    .get_subclass = function(env) {
      env_name <- deparse(substitute(env))
      private$.subclasses[[env_name]] <- env$new()
    },

    .clone_ors_repo = function(dir = NA) {
      basedir <- "openrouteservice-master"
      download_url <- paste0(
        "https://github.com/",
        "GIScience/openrouteservice/archive/refs/heads/master.zip"
      )

      if (missing(dir)) {
        dir <- system.file(package = "ORSRouting")
      }
      cli_abortifnot(dir.exists(dir))

      if (!dir.exists(basedir)) {
        zip_file <- "openrouteservice.zip"
        cli::cli_progress_step(
          "Downloading service backend from GitHub repository...",
          msg_done = "Successfully downloaded the service backend.",
          msg_failed = "Failed to download the service backend."
        )
        download.file(download_url, zip_file)
        unzip(zip_file, exdir = dir)
        file.remove(zip_file)
        cli::cli_progress_done()
      }
      file.path(dir, basedir)
    }
  ),
  cloneable = FALSE
)
