# Title     : Local OpenRouteService backend initialization and control panel
# Objective : Set up, configure and change a local backend of OpenRouteService
# Created by: Jonas Lieth
# Created on: 29.07.2021


options(
  cli.spinner = "dots8",
  cli.spinner_unicode = "dots8",
  cli.spinner_ascii = "dots8"
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
  public = list(
    #' @field dir ORS directory, either passed as a parameter or automatically
    #' set after calling `$new` or `$initialize`.
    dir = NULL,

    #' @description
    #' Initialize \code{\link{ORSInstance}} as well as \code{\link{ORSExtract}},
    #' \code{\link{ORSConfig}} and' \code{\link{ORSDockerInterface}}.
    #' @param dir Custom ORS directory. If not specified, the directory will be
    #' downloaded from the
    #' \href{https://github.com/GIScience/openrouteservice}{official GitHub repository}
    initialize = function(dir = NULL) {
      if (!is.null(dir)) {
        private$.clone_ors_repo(dir)
        self$dir <- dir
      } else {
        private$.clone_ors_repo()
        self$dir <- getwd()
      }
      self$extract <- ORSExtract$new()
      self$get_config()
      self$init_docker()
    },

    #' @description
    #' If necessary, stops the current ORS container when the class instance is
    #' removed.
    finalize = function() {
      if (!is.null(self$docker) && self$docker$container_running) {
        self$docker$stop_container()
      }
    },

    #' @field extract `ORSExtract` environment. See \code{\link{ORSExtract}}.
    extract = NULL,

    #' @field config `ORSConfig` environment. See \code{\link{ORSConfig}}.
    config = NULL,

    #' @description
    #' Initializes \code{\link{ORSConfig}} as an environment field. Call this
    #' if the config path changes (e.g. after the initial ORS setup).
    get_config = function() {
      self$config <- ORSConfig$new()
    },

    #' @field docker `ORSDockerInterface` environment. See
    #' \code{\link{ORSDockerInterface}}.
    docker = NULL,

    #' @description
    #' Initializes \code{\link{ORSDockerInterface}} as an environment field.
    #' This method starts docker and delivers and interface to interact with
    #' docker.
    init_docker = function() {
      if (!is.null(self$setup_ettings)) {
        port <- self$setup_settings$ors_config$services$`ors-app`$ports[1] %>%
          strsplit(":") %>%
          unlist() %>%
          unique() %>%
          as.numeric()
      } else {
        port <- 8080
      }
      self$docker <- ORSDockerInterface$new(port = port)
    },

    #' @field setup_settings `ORSSetupSettings` environment. See
    #' \code{\link{ORSSetupSettings}}.
    setup_settings = NULL,

    #' @description
    #' Initializes \code{\link{ORSSetupSettings}} as an environment field and
    #' prepares the necessary changes to the `Dockerfile` and
    #' `docker-compose.yml`
    #' @param init_memory Initial memory to be allocated to the docker
    #' container.
    #' @param max_memory Maximum memory to be allocated to the docker
    #' container. The container will start with the initial memory and increases
    #' the memory usage up to the maximum memory if necessary.
    get_setup_settings = function() {
      self$setup_settings <- ORSSetupSettings$new(
          self$extract$path,
          self$config$active_profiles
      )
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
    #' @param snapping_radius Integer. Radius within which points are snapped
    #' to the nearest roads. See details.
    #' @param wait Logical. If `TRUE`, the function will not stop running after
    #' the container is being started and will give out a notification as soon
    #' as the service is ready. If `FALSE`, the function will start the
    #' container and then stop. To check the server status, you can then call
    #' `$service_ready` from the class \code{\link{ORSDockerInterface}}. Passed
    #' to \code{\link{ORSDockerInterface}}.
    #' @param run Locial. If `TRUE`, returns `TRUE` if the initial setup is done
    #' and runs the setup if not. If `FALSE`, only returns logicals to check
    #' whether the initial setup is done or not.
    #' @details The default setup removes the maximum snapping radius meaning
    #' that points are snapped to the nearest road irrespective of their
    #' distance to the road. The ORS default lies at 350 meters. Limitting the
    #' snapping radius might cause errors for points that are over 350 meters
    #' from the nearest road. However, removing this limit might cause
    #' inaccuracies in the distance and time calculations as points can be
    #' instantly snapped to a road kilometers away.
    init_setup = function(
      profiles = "car",
      extract_path = self$extract$path,
      init_memory = NULL,
      max_memory = NULL,
      snapping_radius = -1,
      wait = TRUE,
      run = TRUE) {
        if (
          (self$docker$service_ready == "TRUE" ||
          self$docker$image_built == "TRUE" ||
          dir.exists("docker/graphs")) &&
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

        # Initialize config ---------------------------------------------------
        self$get_config()
        self$
          config$
          ors_config$
          ors$
          services$
          routing$
          profiles$
          active <- as.list(profiles)
        self$
          config$
          ors_config$
          ors$
          services$
          routing$
          profiles$
          default_params$
          maximum_snapping_radius <- snapping_radius
        self$
          config$
          ors_config$
          ors$
          services$
          routing$
          profiles$
          `profile-car`$
          parameters$
          maximum_snapping_radius <- snapping_radius
        self$config$save_config()

        # Set up Docker -------------------------------------------------------
        self$get_setup_settings()
        self$setup_settings$graph_building <- "build"
        self$setup_settings$allocate_memory(init_memory, max_memory)
        self$setup_settings$save_compose()

        # Start the service ---------------------------------------------------
        self$init_docker()
        self$docker$image_up(wait)

        # Update the config file
        self$get_config()

        # Turn off graph building
        self$setup_settings$graph_building <- NA
    }
  ),
  private = list(
    .set_ors_wd = function(dir = NULL) {
      basedir <- "openrouteservice-master"
      pkg_path <- system.file(package = "ORSRouting")
      if(dir.exists(pkg_path)) {
        setwd(pkg_path)
      }
      # If no path is given, set the default path as the working dir
      if (is.null(dir) && !grepl(basedir, getwd())) {

         # If both directories exist, set both as the working dir
        if (dir.exists(basedir)) {
          setwd(basedir)
        # If only the first exists, don't do anything and print a warning
        } else {
            cli::cli_abort(
              paste(
               "The OpenRouteService directory does not seem to exist.",
               "Verify that the service backend was downloaded or pass a",
               "custom directory."
              )
            )
        }
      # If a path is passed that is not part of the current working directory
      } else if (!is.null(dir) && !grepl(dir, getwd())) {
        if (dir.exists(dir)) {
          setwd(dir)
        } else {
          cli::cli_abort("The custom directory does not seem to exist.")
        }
      # If the ORS path or one of its children is set as a working directory
      } else {
        # go up the directories until the ORS path is the current working
        # directory
        while (tail(unlist(strsplit(getwd(), "/")), 1) != basedir) {
          setwd("..")
        }
      }
    },
    .dir_download_url = paste0(
      "https://github.com/",
      "GIScience/openrouteservice/archive/refs/heads/master.zip"
    ),
    .clone_ors_repo = function(dir = getwd()) {
      basedir <- "openrouteservice-master"
      if (!missing(dir) && dir.exists(dir)) {
        setwd(dir)
      } else {
        pkg_path <- system.file(package = "ORSRouting")
        if(dir.exists(pkg_path)) {
          dir <- pkg_path
          setwd(dir)
        }
      }
      if (!dir.exists(basedir) && !grepl(basedir, getwd())) {
        zip_file <- "openrouteservice.zip"
        cli::cli_progress_step(
          "Downloading service backend from GitHub repository...",
          msg_done = "Successfully downloaded the service backend.",
          msg_failed = "Failed to download the service backend."
        )
        download.file(private$.dir_download_url, zip_file)
        unzip(zip_file, exdir = dir)
        file.remove(zip_file)
        cli::cli_progress_done()
      }
      private$.set_ors_wd()
    }
  ),
  cloneable = FALSE
)
