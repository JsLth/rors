# Title     : Local OpenRouteService backend initialization and control panel
# Objective : Set up, configure and change a local backend of OpenRouteService
# Created by: Jonas Lieth
# Created on: 29.07.2021



#' OpenRouteService backend control panel
#' @description R6 class that acts as a setup wizard and control panel for the OpenRouteService backend service.
#' The class facilitates the setup of the Docker container and allows making changes to the setup from within R.
#'
#' @details The purpose of this class is to facilitate the OpenRouteService installation process.
#' Alternatively, you can follow the official instructions from the \href{https://giscience.github.io/openrouteservice/installation/Advanced-Docker-Setup.html}{OpenRouteService documentation}.
#' The developer team recently extended the installation guide considerably.
#'
#' The class has four sub classes. \code{\link{ORSExtract}} manages the OpenRouteService extract and is able to
#' download `.pbf` files from different sources using the `osmextract` package. \code{\link{ORSConfig}} controls
#' the configuration file (`ors-config.json`) which is also used to set active profiles.
#' \code{\link{ORSSetupSettings}} can be used to make changes to the Docker setup, e.g. to allocate RAM, assign
#' extracts or change the local server access. \code{\link{ORSDockerInterface}} provides a basic interface to Docker
#' commands and can be used to check the status of the image, container and service. \code{\link{ORSSetupSettings}}
#' should be initialized last as the Docker setup needs information on the extract and the number of
#' profiles to assign the extract and estimate the required RAM to be allocated.
#'
#' If the setup keeps failing due to whatever reason, try resetting the docker path of the main directory
#' (as specified in `$dir`), or just delete the directory and download it again to be safe.
#'
#' If the setup fails due to an OutOfMemoryError, first check if you allocated enough memory. If it keeps
#' failing, clear the available memory or restart the system. OpenRouteService recommends allocating a
#' little more than twice the extract size. Make sure not to allocate more than your available memory.
#' If you allocate more than 80% of your free working memory, the function will stop. For details refer
#' to the \href{https://giscience.github.io/openrouteservice/installation/System-Requirements.html}{system requirements of OpenRouteService}
#'
#' @seealso \code{\link{ORSExtract}}, \code{\link{ORSConfig}}, \code{\link{ORSDockerInterface}}, \code{\link{ORSSSetupSettings}}
#'
#' @importFrom magrittr %>%
#'
#' @export

ORSInstance <- R6::R6Class(
  classname = 'ORSInstance',
  public = list(
    #' @field dir ORS directory, either passed as a parameter or automatically set after
    #' calling `$clone_ors_repo`
    dir = NULL,

    #' @description
    #' Initialize \code{\link{ORSInstance}} as well as \code{\link{ORSExtract}} and \code{\link{ORSConfig}} and 
    #' \code{\link{ORSDockerInterface}}
    #' @param dir Custom ORS directory. If not specified, the directory will be downloaded
    #' from the [official GitHub repository]: https://github.com/GIScience/openrouteservice.
    initialize = function(dir = NULL) {
      if(!is.null(dir)) {
        private$clone_ors_repo(dir)
        self$dir <- dir
      } else {
        private$clone_ors_repo()
        self$dir <- getwd()
      }
      self$extract <- ORSExtract$new()
      self$get_config()
      self$init_docker()
    },

    #' @description
    #' If necessary, stops the current ORS container when the class instance is removed.
    finalize = function() {
      if(!is.null(self$docker) && self$docker$container_running) {
        self$docker$stop_container()
      }
    },

    #' @field extract `ORSExtract` environment. See \code{\link{ORSExtract}}.
    extract = NULL,

    #' @field config `ORSConfig` environment. See \code{\link{ORSConfig}}.
    config = NULL,

    #' @description
    #' Initializes \code{\link{ORSConfig}} as an environment field. Call this if the config path changes
    #' (e.g. after the initial ORS setup).
    get_config = function() {
      self$config <- ORSConfig$new()
    },

    #' @field docker `ORSDockerInterface` environment. See \code{\link{ORSDockerInterface}}.
    docker = NULL,

    #' @description
    #' Initializes \code{\link{ORSDockerInterface}} as an environment field. This method starts docker 
    #' and delivers and interface to interact with docker.
    init_docker = function() {
      if(!is.null(self$setup_ettings)) {
        port <- self$setup_settings$ors_config$services$`ors-app`$ports[1] %>%
          strsplit(':') %>%
          unlist() %>%
          unique() %>%
          as.numeric()
      } else {
        port <- 8080
      }
      self$docker <- ORSDockerInterface$new(port = port)
    },

    #' @field setup_settings `ORSSetupSettings` environment. See \code{\link{ORSSetupSettings}}.
    setup_settings = NULL,

    #' @description
    #' Initializes \code{\link{ORSSetupSettings}} as an environment field and prepares the necessary
    #' changes to the `Dockerfile` and `docker-compose.yml`
    #' @param init_memory Initial memory to be allocated to the docker container.
    #' @param max_memory Maximum memory to be allocated to the docker container. The
    #' container will start with the initial memory and increases the memory usage up to
    #' the maximum memory if necessary.
    get_setup_settings = function(init_memory = NULL, max_memory = NULL) {
      self$setup_settings <- ORSSetupSettings$new(extract_name = basename(self$extract$path),
                                                  init_memory = init_memory,
                                                  max_memory = max_memory,
                                                  profiles = self$config$active_profiles
      )
    },

    #' @description Changes the necessary settings and configurations for the first startup, builds the
    #' image and starts the container. This function should only be used when starting the service for
    #' the first time. Changes after that should preferably be made manually.
    #' @param wait Logical. If `TRUE`, the function will not stop running after the container is being
    #' started and will give out a notification as soon as the service is ready. If `FALSE`, the function
    #' will start the container and then stop. To check the server status, you can then call `$server_ready`
    #' from the class \code{\link{ORSDockerInterface}}.
    init_setup = function(wait = TRUE) {
      # TODO: Implement method and set to TRUE if setup is successful
    }
  ),
  private = list(
    wait = NULL, # TODO: Implement wait parameter
    set_ors_wd = function(dir = NULL) {
      basedir <- 'openrouteservice-master'
      # If no path is given, set the default path as the working dir
      if(is.null(dir) && !grepl(basedir, getwd())) {
         # If both directories exist, set both as the working dir
         if(dir.exists(basedir)) {
           setwd(basedir)
         # If only the first exists, don't do anything and print a warning
         } else {
           cli::cli_abort('The OpenRouteService directory does not seem to exist. Verify that the service backend was downloaded or pass a custom directory.')
         }
      # If a path is passed that is not part of the current working directory
      } else if(!is.null(dir) && !grepl(dir, getwd())) {
        if(dir.exists(dir)) {
          setwd(dir)
        } else {
          cli::cli_abort('The custom directory does not seem to exist.')
        }
      # If the ORS path or one of its children is set as a working directory
      } else {
        # go up the directories until the ORS path is the current working directory
        while(tail(unlist(strsplit(getwd(), '/')), 1) != basedir) {
          setwd('../../GeoTools')
        }
      }
    },
    dir_download_url = 'https://github.com/GIScience/openrouteservice/archive/refs/heads/master.zip',
    clone_ors_repo = function(dir = getwd()) {
      basedir <- 'openrouteservice-master'
      if(!is.null(dir) && dir.exists(dir)) {
        setwd(dir)
      }
      if(!dir.exists(basedir) && !grepl(basedir, getwd())) {
        zip_file <- 'openrouteservice.zip'
        cli::cli_progress_step('Downloading service backend from GitHub repository...')
        download.file(private$dir_download_url, zip_file)
        unzip(zip_file, exdir = dir)
        file.remove(zip_file)
        cli::cli_progress_update()
      }
      private$set_ors_wd()
    }
  ),
  cloneable = FALSE
)


#' OpenRouteService OSM extract control panel
#' @description R6 class to download, set or manage an OpenStreetMap extract
#'
#' @details Note that the coverage of the OSM extract should include all necessary places that need to
#' be processed. If a location is not covered by the extract, the service will respond with an error.
#' If necessary, the OSM extract can be changed later by running `$assign_data(build = change)` in
#' \code{\link{ORSSetupSettings}}.
#'
#' @importFrom magrittr %>%

ORSExtract <- R6::R6Class(
  classname = 'ORSExtract',
  public = list(

    #' @field path Relative path to the extract.
    path = NULL,

    #' @field place Name or coordinates of the place that is passed for the extract download.
    place = NULL,

    #' @field level Hierarchical level of the extraxt to be matched.
    level = NULL,

    #' @field size Size of the extract file in megabytes
    size = NULL,

    #' @field provider Provider of the extract size.
    provider = NULL,

    #' @description Initializes \code{\link{ORSExtract}} and looks for an existing extract in data path.
    initialize = function() {
      osm_file_occurences <- dir('docker/data') %>%
        grepl('.pbf|.osm.gz|.osm.zip|.osm', .)
      if(sum(osm_file_occurences) == 1) {
        path <- paste0('docker/data/', dir('docker/data')[osm_file_occurences])
        self$path <- path
        self$size <- file.info(path)$size * 0.000001
      } else if(sum(osm_file_occurences) > 1) {
        cli::cli_alert_warning('There a multiple OSM files in the data directory. Please set an extract manually.')
      }
    },

    #' @description Downloads an OSM extract.
    #' @param place Character scalar, sf or sfc object or length-2 numeric
    #' vector to be passed to \code{\link[osmextract]{oe_match}}. Represents
    #' a place that falls inside the coverage of defined extract regions of
    #' the providers listed in \code{\link[osmextract]{oe_providers}}. The
    #' geographic scale can be adjusted by changing the parameter `level`. For
    #' details, refer to \code{\link[osmextract]{oe_match}}.
    #' @param level Passed to \code{\link[osmextract]{oe_match}}.
    #' @details The extract is downloaded directly to docker/data. This will
    #' also be the directory that is passed to the
    #' \code{\link{ORSSetupSettings}} to process the extract. This directory is
    #' not mutable because Docker expects a relative path to its main directory.
    get_extract = function(place, ...) {
      download_path <- 'docker/data'
      ok <- TRUE
      i <- 0
      providers <- suppressMessages({osmextract::oe_providers()$available_providers})
      cli::cli_alert_info('Trying different extract providers...')
      while(ok && i < length(providers)) {
        i <- i + 1
        place_match <- osmextract::oe_match(place, provider = providers[i], quiet = TRUE, ...)
        file_name <- strsplit(place_match$url, '/') %>% .[[1]] %>% tail(1)
        cli::cli_alert_info('The extract {.val {file_name}} is {.val {round(place_match$file_size / 1024 / 1024)}} MB in size and will be downloaded from {.val {providers[i]}}.')
        if(providers[i] == 'bbbike') {
          cli::cli_alert_warning('bbbike extracts are known to cause issues with memory allocation. Use with caution.')
        }
        input <- tolower(readline('Should a different provider be tried? (Yes/No/Cancel)'))
        if(!input %in% c('yes', 'no')) {
          cli::cli_alert_danger('Function cancelled.')
          invokeRestart('abort')
        }
        ok <- input == 'yes'
      }
      if(ok) {
        cli::cli_alert_warning('All providers have been searched. Please download the extract manually.')
        invokeRestart('abort')
      }
      file_occurences <- grepl(file_name, dir(download_path))
      if(sum(file_occurences) == 1) {
        cli::cli_alert_info('The extract already exists in the download path. Download will be skipped.')
        path <- paste(download_path, dir(download_path)[file_occurences], sep ='/')
        cli::cli_text('Download path: {.val {path}}')
      } else {
        private$rm_old_extracts()
        path <- paste0('docker/data/', providers[i], '_', file_name)
        cli::cli_progress_step('Downloading the OSM extract...',
                               msg_done = 'The extract was successfully downloaded to the following path: {.val {path}}',
                               msg_failed = 'Extract could not be downloaded.')
        osmextract::oe_download(place_match$url,
                                        provider = providers[i],
                                        download_directory = normalizePath(download_path, winslash = '/'),
                                        quiet = TRUE)

      }
      size <- file.info(path)$size / 1024 / 1024
      if(size >= 6000) {
        cli::cli_alert_warning('The OSM extract is very large. Make sure that you have enough working memory available.')
      }
      self$path <- path
      self$place <- place
      self$level <- level
      self$size <- size
      self$provider <- providers[i]
    },

    #' @description Moves a given OSM extract to the ORS data directory
    #' @param extract_path Character scalar. Path to an OSM extract formatted as `.pbf`, `.osm`, `.osm.gz`
    #' or `.osm.zip`.
    set_extract = function(extract_path) {
      if(file.exists(extract_path)) {
        self$path <- relativePath(private$move_extract(extract_path))
        self$size <- file.info(extract_path)$size * 0.000001
      } else {
        cli::cli_abort('The provided extract file does not exist.')
      }
    }
  ),
  private = list(
    move_extract = function(extract_path) {
      # Derive file name from file path
      file_name <- extract_path %>%
      strsplit('/') %>%
      unlist() %>%
      tail(1)

      # Move extract to ./docker/data
      file.copy(extract_path, 'docker/data')
      paste('docker/data', file_name, sep = '/')
    },
    rm_old_extracts = function() {
      path <- normalizePath('docker/data', winslash = '/')
      extract_occurences <- dir(path) %>%
        grepl('.pbf|.osm.gz|.osm.zip|.osm', .)
      if(sum(extract_occurences) > 0) {
        cli::cli_alert_info('Removing old extracts...')
        for(extract in dir(path)[extract_occurences]) {
          file.remove(paste0('docker/data/', extract))
        }
      }
    }
  ),
  cloneable = FALSE
)


#' OpenRouteService configuration control panel
#' @description R6 class that loads the ORS config file and can be used to change the ORS configurations
#'
#' @details The argument `profiles` refers to the supported modes of transport. Avoid passing all profiles
#' as each profile has to be built seperately, which can strain memory extremely quickly. For a list of
#' and details on the supported profiles, refer to the \href{https://giscience.github.io/openrouteservice/documentation/Tag-Filtering.html}{OpenRouteService documentation}.
#' For a details on each configuration in the config file, refer to the \href{https://giscience.github.io/openrouteservice/installation/Configuration.html}{config documentation}.
#'
#' @seealso \code{\link{ORSInstance}}
#'
#' @importFrom magrittr %>%

ORSConfig <- R6::R6Class(
  classname = 'ORSConfig',
  active = list(

    #' @field active_profiles Currently active profiles in the config file. By assigning a character
    #' vector, the field changes the active profiles in the config file.
    active_profiles = function(profiles) {
      if(missing(profiles)) {
        self$ors_config$ors$services$routing$profiles$active
      } else {
        if(is.character(profiles)) {
          profiles <- private$translate_profiles(profiles) %>%
            .[!duplicated(.)]
          self$ors_config$ors$services$routing$profiles$active <- profiles
        } else {
          cli::cli_abort('Pass a valid vector of profiles.')
        }
      }
    }
  ),
  public = list(

    #' @field ors_config JSON config file, parsed as a list. Objects and items can be changed by assigning
    #' values to them.
    ors_config = NULL,

    #' @field path Path to the config file. Usually, this is either docker/conf if the ORS image was
    #' already built or docker/data if the image is yet to be built for the first time.
    path = NULL,

    #' @description Initializes the `ORSConfig` class. Specifies the config path, copies the config files
    #' if necessary and reads them.
    initialize = function() {
      if(basename(getwd()) == 'openrouteservice-master') {
        if(dir.exists('docker/conf')) {
          config <- jsonlite::read_json('docker/conf/ors-config.json')
          self$path <- 'docker/conf/ors-config.json'
        } else if(file.exists('docker/data/ors-config.json')) {
          config <- jsonlite::read_json('docker/data/ors-config.json')
          self$path <- 'docker/data/ors-config.json'
        } else {
          file.copy('openrouteservice/src/main/resources/ors-config-sample.json',
                    'docker/data')
          file.rename('docker/data/ors-config-sample.json', 'docker/data/ors-config.json')
          config <- jsonlite::read_json('docker/data/ors-config.json')
          self$path <- 'docker/data/ors-config.json'
        }
        self$ors_config <- config
        self$save_config()
      } else {
        cli::cli_abort('This class must be initialized from the ORS main directory.')
      }
      invisible(self)
    },

    #' @description Saves the config changes by overwriting the config files with all changed fields.
    #' This should be run each time after changing any configurations.
    save_config = function() {
      config_json <- jsonlite::toJSON(self$ors_config, auto_unbox = TRUE, pretty = TRUE)
      if(dir.exists('docker/conf')) {
        cat(config_json, file = 'docker/conf/ors-config.json')
      } else {
        cat(config_json, file = 'docker/data/ors-config.json')
      }
    },

    #' @description Opens the raw config file to allow manual changes. Useful if you find the list
    #' structure of the parsed JSON impractical.
    open_config = function() {
      shell(normalizePath(self$path, winslash = '\\'))
    }
  ),
  private = list(
    translate_profiles = function(profiles) {
      translator <- data.frame(
        normal_names = c(
          'driving-car', 'driving-hgv', 'cycling-regular', 'cycling-mountain', 'cycling-road',
          'cycling-electric', 'foot-walking', 'foot-hiking', 'wheelchair'
        ),
        config_names = c(
          'car', 'hgv', 'bike-regular', 'bike-mountain', 'bike-road', 'bike-electric',
          'walking', 'hiking', 'wheelchair'
        )
      )
      translate <- function(profile) {
        if(profile %in% translator$normal_names) {
          translator$config_names[translator$normal_names == profile]
        } else if(profile %in% translator$config_names) {
          profile
        } else {
          cli::cli_warn('Profile {.val {profile}} does not conform to the ORS naming scheme and will be skipped.')
          NULL
        }
      }
      translated_profiles <- lapply(profiles, translate) %>%
        purrr::discard(is.null) %>%
        unname()
    }
  ),
  cloneable = FALSE
)


#' R6 Docker setup control panel
#' @description R6 class that controls `docker-compose.yml` and `Dockerfile`. Provides an interface to
#' easily allocate memory, switch graph building on or off and assign data.
#'
#' @importFrom magrittr %>%

ORSSetupSettings <- R6::R6Class(
  classname = 'ORSSetupSettings',
  active = list(

    #' @field build_graphs Switches graph building on or off. Graph building should be turned on when
    #' the OSM extract changes or when no graphs exist yet. It should be turned off after building
    #' graphs. If graph building is turned on after building the graphs, the service will rebuild all
    #' graphs each time the image is rebuilt.
    build_graphs = function(order) {
      if(missing(order)) {
        build_graphs_string <- self$compose$services$`ors-app`$environment[1]
        order <- build_graphs_string %>%
          strsplit('=') %>%
          unlist() %>%
          .[2] %>%
          as.logical()
        return(order)
      } else {
        if(is.logical(order)) {
          build_graphs_string <- private$stringify_graph_building(order)
          self$compose$services$`ors-app`$environment[1] <- build_graphs_string
          return(order)
        } else {
          cli::cli_abort('Pass a logical scalar. Should graphs be built or not?')
        }
      }
    }
  ),
  public = list(

    #' @field compose `docker-compose.yml`, parsed as a list. Blocks and items can be changed by assigning
    #' values to them.
    compose = NULL,

    #' @field init_memory Initial memory to be allocated to the docker container. The container will
    #' start with this amount of memory and will increase its memory usage if necessary.
    init_memory = NULL,

    #' @field max_memory Maximum memory to be allocated to the docker container. The container is not
    #' allowed to exceed this memory limit.
    max_memory = NULL,

    #' @field extract_path Path where the OSM extract is saved.
    extract_path = NULL,

    #' @field config_path Path where the config file is saved.
    config_path = NULL,

    #' @description Initializes the `ORSSetupSettings` class. Reads the `docker-compose.yml`, adjusts the
    #' `Dockerfile` and allocates memory.
    #' @param extract_name File name of the extract
    #' @param init_memory Initial memory to be allocated to the docker container.
    #' @param max_memory Maximum memory to be allocated to the docker container. The
    #' container will start with the initial memory and increases the memory usage up to
    #' the maximum memory if necessary.
    #' @param profiles Active profiles as set in the config file
    initialize = function(extract_name, init_memory = NULL, max_memory = NULL, profiles = NULL) {
      self$compose <- private$read_dockercompose()
      self$extract_path <- paste('docker/data', extract_name, sep = '/')
      self$config_path <- 'docker/data/ors-config.json'
      private$extract_size <- file.info(self$extract_path)$size * 0.000001
      private$profiles <- length(profiles)
      private$disable_auto_deletion()
      self$allocate_memory(init_memory, max_memory)
      self$save_settings()
      invisible(self)
    },

    #' @description Specifies the amount of memory to be allocated. If only the memory limit is given,
    #' the initial memory will be set to half that amount. If no memory is given, the method will
    #' estimate the optimal amount of memory to be allocated. The memory is written to the compose file
    #' @param init Initial memory to be allocated to the docker container.
    #' @param max Maximum memory to be allocated to the docker container. The
    #' container will start with the initial memory and increases the memory usage up to
    #' the maximum memory if necessary.
    allocate_memory = function(init = NULL, max = NULL) {
      if(is.numeric(init) && is.numeric(max)) {
        private$write_memory(init, max)
      } else if(is.numeric(init) && is.null(max)) {
        max <- init
        private$write_memory(init, max)
      } else if(is.null(init) && is.numeric(max)) {
        init <- max / 2
        private$write_memory(init, max)
      } else if(is.null(init) && is.null(max)) {
        if(!is.null(private$extract_size) && !is.null(private$profiles)) {
          max <- round(private$extract_size, -2) * 2.5 * private$profiles
          init <- max / 2
          private$write_memory(init, max)
        } else {
          cli::cli_abort('Initialize the extract and config before changing the setup settings or pass a fixed amount of memory. The memory estimation is based on the extract size and the number of active profiles.')
        }
      } else {
        cli::cli_abort('Either pass a numeric or nothing.')
      }
      gc()
      free_mem <- memuse::Sys.meminfo()$freeram@size
      if(free_mem * 0.8 - max <= 0) {
        cli::cli_warn('You are allocating more than your available memory. Consider lowering the allocated RAM.')
      }
      self$init_memory <- paste(as.character(init / 1000), 'GB')
      self$max_memory <- paste(as.character(max / 1000), 'GB')
    },

    #' @description Writes the provided paths to the compose file.
    #' @param mode Specifices whether the image is built for the first time or if the OSM extract is
    #' being changed. `mode = build` inserts a `build` block that specifices the path to the extract
    #' and config file. `mode = change` inserts a volume that overwrites the current graphs if graph
    #' building is turned on.
    assign_data = function(mode = 'build') {
      self$compose$services$`ors-app`$build <- NULL
      self$compose$services$`ors-app`$volumes <- self$compose$services$`ors-app`$volumes[-6]
      if(mode == 'build') {
        build_branch <- list(
          build = list(
            context = '../',
            args = list(
              APP_CONFIG = './%s' %>% sprintf(self$config_path),
              OSM_FILE = './%s' %>% sprintf(self$extract_path)
            )
          )
        )
        self$compose$services$`ors-app` <- append(self$compose$services$`ors-app`,
                                             build_branch,
                                             after = 3)
      } else if(mode == 'change') {
        change_node <- './%s:/ors-core/data/osm_file.pbf' %>% sprintf(self$extract_path)
        self$compose$services$`ors-app`$volumnes[6] <- change_node
      }
    },

    #' @description Saves the setup changes by overwriting `docker-compose.yml` with all changed fields.
    #' This should be run each time after changing any settings.
    save_settings = function() {
      private$write_dockercompose()
    },

    #' @description Opens the raw compose file to allow manual changes. Useful if you find the list
    #' structure of the parsed yaml impractical.
    open_config = function() {
      shell(normalizePath('docker/docker-compose.yml', winslash = '\\'))
    }
  ),
  private = list(
    extract_size = NULL,
    profiles = NULL,
    write_memory = function(init, max) {
      java_options <- self$compose$services$`ors-app`$environment[2]
      java_mem <- strsplit(java_options, ' ') %>%
        unlist() %>%
        tail(2)
      init_mem_allocation <- java_mem[1]
      max_mem_allocation <- java_mem[2]
      java_options <- gsub(init_mem_allocation, sprintf('-Xms%sm', init), java_options)
      java_options <- gsub(max_mem_allocation, sprintf('-Xmx%sm', max), java_options)
      self$compose$services$`ors-app`$environment[2] <- java_options
    },
    stringify_graph_building = function(order) {
      order <- tolower(as.character(order))
      substr(order, 1, 1) <- toupper(substr(order, 1, 1))
      build_graphs_string <- 'BUILD_GRAPHS=%s' %>% sprintf(order)
    },
    disable_auto_deletion = function() {
      # Don't delete any profiles. Setup every profile at first start.
      dockerfile <- readLines('Dockerfile', warn = FALSE)
      delete_line <- grep('Delete all profiles but car', dockerfile)
      if(length(delete_line) > 0) {
        lines_to_be_deleted <- c(delete_line,
                               delete_line + 1,
                               delete_line + 2)
        dockerfile[seq(delete_line-2, delete_line-1)] <- gsub(
        ' && \\\\',
        '',
        dockerfile[seq(delete_line-2, delete_line-1)]
        )
      dockerfile <- dockerfile[-lines_to_be_deleted] %>%
        paste0(collapse = '\n')
      cat(dockerfile, file = 'Dockerfile')
      }
    },
    read_dockercompose = function() {
      yaml::read_yaml('docker/docker-compose.yml')
    },
    write_dockercompose = function() {
      # Put Java and Catalina options in quotes
      self$
        compose$
        services$
        `ors-app`$
        environment[2] <- shQuote(self$compose$services$`ors-app`$environment[2])
      self$
        compose$
        services$
        `ors-app`$
        environment[3] <- shQuote(self$compose$services$`ors-app`$environment[3])

      # Build yaml with indented bullet points
      yml_as_string <- yaml::as.yaml(self$compose, indent.mapping.sequence = TRUE)

      # Remove single quotes that are somehow added by as.yaml when introducing double quotes
      corrected_yml_string <- gsub("\"'|'\"", '"', yml_as_string)

      # Remove line breaks of long strings
      corrected_yml_string <- corrected_yml_string %>%
        gsub('\\n\\s{8}-', ' -', .)

      # Write new yaml to old yaml file
      cat(corrected_yml_string, file = 'docker/docker-compose.yml')
    }
  ),
  cloneable = FALSE
)


#' OpenRouteService Docker interface
#' @description R6 class that interacts with the Docker daemon and is able to run basic commands
#' on the openrouteservice:latest image and the ors-app container
#'
#' @importFrom magrittr %>%

ORSDockerInterface <- R6::R6Class(
  classname = 'ORSDockerInterface',
  active = list(

    #' @field docker_running Checks if the Docker daemon is running.
    docker_running = function(v) {
      if(missing(v)) {
        private$.docker_running()
      } else {
        cli::cli_abort('`$docker_running` is read only.')
      }
    },

    #' @field image_built Checks if the openrouteservice:latest image exists.
    image_built = function(v) {
      if(missing(v)) {
        private$.image_built()
      } else {
        cli::cli_abort('`$image_built` is read only.')
      }
    },

    #' @field container_exists Checks if the container ors-app exists.
    container_exists = function(v) {
      if(missing(v)) {
        private$.container_exists()
      } else {
        cli::cli_abort('`$container_exists` is read only.')
      }
    },

    #' @field container_running Checks if the the container ors-app is running.
    container_running = function(v) {
      if(missing(v)) {
        private$.container_running()
      } else {
        cli::cli_abort('`$container_running` is read only.')
      }
    },

    #' @field service_ready Checks if the container service is ready to use. If this field is `TRUE`,
    #' the service can be queried and used.
    service_ready = function(v) {
      if(missing(v)) {
        private$.service_ready()
      } else {
        cli::cli_abort('`$service_ready` is read only.')
      }
    }
  ),
  public = list(

    #' @description Initializes ORSDockerInterface, starts the Docker daemon and specifies the port.
    #' @param port Integer scalar. Port that the server should run on.
    initialize = function(port = 8080) {
      private$start_docker()
      private$port <- port
      invisible(self)
    },

    #' @description Builds the image, starts the container and issues a system notification when the
    #' service is ready to be used.
    image_up = function() {
      setwd('docker')
      system('docker-compose up -d')
      private$notify_when_ready()
      setwd('..')
    },

    #' @description Deletes the image. Should only be used when the container does not exist.
    image_down = function() {
      setwd('docker')
      system('docker-compose down --rmi \'all\'')
      setwd('..')
    },

    #' @description Starts the container and issues a system notification when the service is ready to
    #' be used.
    start_container = function() {
      setwd('docker')
      invisible(system('docker start ors-app'))
      private$notify_when_ready(interval = 2, shutup = TRUE)
      setwd('..')
    },

    #' @description Stops the container.
    stop_container = function() {
      setwd('docker')
      invisible(system('docker stop ors-app'))
      setwd('..')
    }
  ),
  private = list(
    port = NULL,
    .service_ready = function() {
      health_url <- 'http://localhost:%s/ors/health' %>% sprintf(private$port)
      if(private$.docker_running() &&
         private$.image_built() &&
         private$.container_exists() &&
         private$.container_running()) {
        health <- httr::GET(health_url) %>% httr::content()
        health$status == 'ready'
      } else {
        FALSE
      }
    },
    .container_exists = function() {
      container_exists <- system(
        'docker ps -a --format "{{.Names}}" --filter name=^/ors-app$',
        intern = TRUE
      ) %>%
        identical('ors-app')
    },
    .container_running = function() {
      container_status <- suppressWarnings(
        system(
          'docker container ls -a --format "{{.State}}" --filter name=^/ors-app$',
          intern = TRUE,
          ignore.stderr = TRUE
        )
      ) %>%
        identical('running')
    },
    .image_built = function() {
      image_name <- system(
        'docker images "openrouteservice/openrouteservice" --format {{.Repository}}',
        intern = TRUE)
      if(length(image_name) > 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    .docker_running = function() {
      docker_check <- system('docker ps',
                             ignore.stdout = TRUE,
                             ignore.stderr = TRUE)
      if(docker_check == 0) {
        return(TRUE)
      } else if(docker_check == 1) {
        return(FALSE)
      } else if(docker_check == -1) {
        cli::cli_abort('Docker is not recognized as a command. Is it properly installed?')
      } else {
        cli::cli_abort('Cannot check Docker status for some reason.')
      }
    },
    start_docker = function() {
      if(!private$.docker_running()) {
        docker_path <- system('where docker.exe', intern = TRUE)
        docker_desktop <- docker_path %>%
          strsplit('\\\\') %>%
          unlist() %>%
          head(-3) %>%
          append('Docker Desktop.exe') %>%
          paste(collapse = '/') %>% 
          shQuote()
        scode <- shell(docker_desktop, wait = FALSE)
        # If Docker is installed, it will try to open
        if(scode == 0) {
          cli::cli_progress_step(
            'Starting Docker...', 
            spinner = TRUE,
            msg_done = 'Docker Desktop is now running.',
            msg_failed = 'The Docker startup has timed out.'
          )
          # Check if Docker is usable by running a Docker command
          while(system('docker ps', ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
            for (i in 1:10) {
              cli::cli_progress_update()
              Sys.sleep(0.1)
            }
          }
        } else if(scode == -1) {
          cli::cli_abort('Docker does not seem to be installed on your system.')
        } else {
          # If something else is returned, I'm all out of ideas
          cli::cli_abort('Something went wrong while starting Docker.')
        }
      }
    },
    notify_when_ready = function(interval = 10, shutup = FALSE) {
      # Checks the service status and gives out a visual and audible notification when
      # the server is ready. Also watches out for errors in the log files.
      cli::cli_inform('The container is being set up and started now. You can stop the process now or let it run and get notified when the service is ready.')
      cli::cli_progress_step('Starting service', spinner = TRUE)
      while(!self$service_ready) {
        for(i in seq_len(interval * 10)) {
          cli::cli_progress_update()
          Sys.sleep(0.1)
        }
        errors <- private$watch_for_error()
        if(!is.null(errors)) {
          cli::cli_abort(
            c(
              'The service ran into the following errors:',
              cli::cli_vec(errors, style = list(vec_sep = '\n'))
            )
          )
        }
      }
      if(!shutup) {
        switch(Sys.info()['sysname'],
               Windows = {
                 system("rundll32 user32.dll,MessageBeep -1")
                 system('msg * \"ORS Service is ready!\"')
               },
               Darwin = {
                 system(
                   paste(
                     'osascript -e \'display notification',
                     '\"ORS Service is ready!\"',
                     'with title',
                     '\"Message from R\"'
                   )
                 )
               },
               Linux = {
                 system('notify-send "ORS Service is ready!" "Message from R"')
               }
        )
      }
      invisible(NULL)
    },
    watch_for_error = function() {
      # Searches the OpenRouteService logs for the keyword 'error' and returns their
      # error messages.
      # The function might be called before logs are created. If this is the case, don't stop,
      # just don't return anything.
      if(dir.exists('docker/logs') &&
        is.element(c('ors', 'tomcat'), dir('docker/logs')) &&
        length(dir('docker/logs/ors')) != 0 &&
        length(dir('docker/logs/tomcat')) != 0) {
        logs <- c(
          # Logs from Apache Tomcats web container
          readLines('docker/logs/tomcat/catalina.%s.log' %>% sprintf(Sys.Date())),
          # Logs from Apache Tomcats local host
          readLines('docker/logs/tomcat/localhost.%s.log' %>% sprintf(Sys.Date())),
          # Logs from OpenRouteService (this sometimes stops logging)
          readLines('docker/logs/ors/ors.log'),
          # Output from Dockers logs command (this never stops logging until it's done)
          system2('docker', args = 'logs ors-app', stdout = FALSE, stderr = TRUE)
        )
        error.catcher <- function(log) {
          errors <- grep('error | exception', log, value = TRUE, ignore.case = TRUE) %>%
            unlist()
          error_msgs <- errors %>%
            strsplit(' - ') %>%
            do.call(rbind, .)
          # CLI logs are formatted differently and are therefore not splitted by strsplit.
          # If this is the case, just return the whole thing, else return only the messages.
          if(length(error_msgs) > 1) {
            return(error_msgs[, 2])
          } else {
            return(error_msgs)
          }
        }
        sapply(logs, error.catcher) %>%
          purrr::discard(sapply(., is.null)) %>%
          unlist(use.names = FALSE) %>%
          unique() # Don't return the same errors multiple times
          # The function might be called before log files are created. In this case, don't
          # stop, just don't return anything.
      }
    }
  ),
  cloneable = FALSE
)


translate_os_cmd <- function() {
  os <- Sys.info()['sysname']
  translator <- list(
    Windows = c(
      'where docker'
    ),
    Linux = c(
      'type docker -p'
    ),
    MacOS = c(
      ''
    )
  )
}