# Title     : Docker setup control panel
# Objective : Change to the ORS docker-compose settings
# Created by: Jonas Lieth
# Created on: 17.08.2021


#' R6 Docker setup control panel
#' @description R6 class that controls `docker-compose.yml` and `Dockerfile`.
#' Provides an interface to easily allocate memory, switch graph building on or
#' off and assign data. It is recommended to initialize this class after
#' setting an extract and configuring ORS.
#'
#' @importFrom magrittr %>%

ORSSetupSettings <- R6::R6Class(
  classname = "ORSSetupSettings",
  active = list(
    #' @field Specifices whether the image is built for the first time or if
    #' the OSM extract is being changed. If `build` is assigned to the field,
    #' indiciates that the graphs are built for the first time. If `change` is
    #' assigned, indicates that the extract file was changed and the existing
    #' graphs should be overwritten using the new extract. If `NA` is assigned,
    #' indicates that no changes should be made and that graph building should
    #' not be forced.
    graph_building = function(mode = "build") {
      build <- "build" %in% names(
        self$
          compose$
          services$
          `ors-app`
      )
      change <- !is.na(
        self$
          compose$
          services$
          `ors-app`$
          environment[6]
      )
      if(missing(mode)) {
        if(build) {
          return("build")
        } else if(change) {
          return("change")
        } else {
          return(NA)
        }
      } else {
        if(is.null(self$extract_path)) {
          cli::cli_warn(
            paste(
              "Please set an extract before calling this binding it or assign",
              "a path to {.var $extract_path}"
            )
          )
          return(NA)
        }

        self$
          compose$
          services$
          `ors-app`$
          build <- NULL

        self$
          compose$
          services$
          `ors-app`$
          volumes <- self$compose$services$`ors-app`$volumes[-6]

        if (identical(mode, "build")) {
          private$.force_graphbuilding(handle = FALSE)
          build_branch <- list(
            build = list(
              context = "../",
              args = list(
                APP_CONFIG = "./%s" %>% sprintf(self$config_path),
                OSM_FILE = "./%s" %>% sprintf(self$extract_path)
              )
            )
          )

          self$
            compose$
            services$
            `ors-app` <- append(
            self$compose$services$`ors-app`,
            build_branch,
            after = 3
          )
          return(mode)
        } else if (identical(mode, "change")) {
          private$.force_graphbuilding(handle = TRUE)
          change_node <- "./%s:/ors-core/data/osm_file.pbf" %>%
            sprintf(self$extract_path)

          self$
          compose$
          services$
          `ors-app`$
          volumes[6] <- change_node
          return(mode)
        } else if (is.na(mode) || is.null(mode)) {
          private$.force_graphbuilding(handle = FALSE)
          return(NA)
        } else {
          cli::cli_abort(
            "{.var $graph_building} expects a character scalar or NA"
          )
        }
      }
    }
  ),
  public = list(

    #' @field compose `docker-compose.yml`, parsed as a list. Blocks and items
    #' can be changed by assigning values to them.
    compose = NULL,

    #' @field init_memory Initial memory to be allocated to the docker
    #' container. The container will start with this amount of memory and will
    #' increase its memory usage if necessary.
    init_memory = NULL,

    #' @field max_memory Maximum memory to be allocated to the docker container.
    #' The container is not allowed to exceed this memory limit.
    max_memory = NULL,

    #' @field extract_path Path where the OSM extract is saved.
    extract_path = NULL,

    #' @field config_path Path where the config file is saved.
    config_path = NULL,

    #' @description Initializes the `ORSSetupSettings` class. Reads the
    #' `docker-compose.yml`, adjusts the `Dockerfile` and allocates memory.
    #' @param extract_name File name of the extract
    #' @param init_memory Initial memory to be allocated to the docker
    #' container.
    #' @param max_memory Maximum memory to be allocated to the docker
    #' container. The container will start with the initial memory and
    #' increases the memory usage up to the maximum memory if necessary.
    #' @param profiles Active profiles as set in the config file
    initialize = function(
      extract_path,
      profiles = NULL
    ) {
      self$compose <- private$.read_dockercompose()
      if(!is.null(extract_path)) {
        self$extract_path <- extract_path
        private$.extract_size <- file.info(self$extract_path)$size * 0.000001
      }
      if (!is.null(profiles)) {
        private$.profiles <- length(profiles)
      }
      self$config_path <- "docker/data/ors-config.json"
      private$.disable_auto_deletion()
      return(self)
    },

    #' @description Specifies the amount of memory to be allocated. If only the
    #' memory limit is given, the initial memory will be set to half that
    #' amount. If no memory is given, the method will estimate the optimal
    #' amount of memory to be allocated. The memory is written to the compose
    #' file.
    #' @param init Initial memory (GB) to be allocated to the docker container.
    #' @param max Maximum memory (GB) to be allocated to the docker container.
    #' The' container will start with the initial memory and increases the
    #' memory usage up to the maximum memory if necessary.
    allocate_memory = function(init = NULL, max = NULL) {
      if (is.numeric(init) && is.numeric(max)) {
        private$.write_memory(init, max)
      } else if (is.numeric(init) && is.null(max)) {
        max <- init
        private$.write_memory(init, max)
      } else if (is.null(init) && is.numeric(max)) {
        init <- max / 2
        private$.write_memory(init, max)
      } else if (is.null(init) && is.null(max)) {
        if (!is.null(private$.extract_size) && !is.null(private$.profiles)) {
          max <- round(private$.extract_size, -2) * 2.5 * private$.profiles / 1000
          init <- max / 2
          private$.write_memory(init, max)
        } else {
          cli::cli_abort(
            paste(
              "Initialize the extract and config before changing the setup",
              "settings or pass a fixed amount of memory. The memory",
              "estimation is based on the extract size and the number of",
              "active profiles."
            )
          )
        }
      } else {
        cli::cli_abort("Either pass a numeric or nothing.")
      }
      gc(verbose = FALSE)
      free_mem <- memuse::Sys.meminfo()$freeram@size
      if (free_mem * 0.8 - max <= 0) {
        cli::cli_warn(
          paste(
            "You are allocating more than your available memory.",
            "Consider lowering the allocated RAM."
          )
        )
      }
      self$init_memory <- paste(as.character(init), "GB")
      self$max_memory <- paste(as.character(max), "GB")
    },

    #' @description Saves the setup changes by overwriting `docker-compose.yml`
    #' with all changed fields. This should be run each time after changing any
    #' settings.
    save_compose = function() {
      private$.write_dockercompose()
    },

    #' @description Opens the raw compose file to allow manual changes. Useful
    #' if you find the list structure of the parsed yaml impractical.
    open_compose = function() {
      shell(normalizePath("docker/docker-compose.yml", winslash = "\\"))
    }
  ),
  private = list(
    .extract_size = NULL,
    .profiles = NULL,
    .write_memory = function(init, max) {
      java_options <- self$compose$services$`ors-app`$environment[2]
      java_mem <- strsplit(java_options, " ") %>%
        unlist() %>%
        tail(2)
      init_mem_allocation <- java_mem[1]
      max_mem_allocation <- java_mem[2]
      java_options <- gsub(
        init_mem_allocation,
        sprintf("-Xms%sm", init),
        java_options
      )
      java_options <- gsub(
        max_mem_allocation,
        sprintf("-Xmx%sm", max),
        java_options
      )
      self$compose$services$`ors-app`$environment[2] <- java_options
    },
    .force_graphbuilding = function(handle) {
      handle <- tolower(as.character(handle))
      substr(handle, 1, 1) <- toupper(substr(handle, 1, 1))
      build_graphs_string <- "BUILD_GRAPHS=%s" %>% sprintf(handle)
      self$
        compose$
        services$
        `ors-app`$
        environment[1] <- build_graphs_string
    },
    .disable_auto_deletion = function() {
      # Don't delete any profiles. Setup every profile at first start.
      dockerfile <- readLines("Dockerfile", warn = FALSE)
      delete_line <- grep("Delete all profiles but car", dockerfile)
      if (length(delete_line) > 0) {
        lines_to_be_deleted <- c(delete_line,
                               delete_line + 1,
                               delete_line + 2)
        dockerfile[seq(delete_line - 2, delete_line - 1)] <- gsub(
        " && \\\\",
        "",
        dockerfile[seq(delete_line - 2, delete_line - 1)]
        )
      dockerfile <- dockerfile[-lines_to_be_deleted] %>%
        paste0(collapse = "\n")
      cat(dockerfile, file = "Dockerfile")
      }
    },
    .read_dockercompose = function() {
      yaml::read_yaml("docker/docker-compose.yml")
    },
    .write_dockercompose = function() {
      # Preserve options before they get put in quotes to not mess up the
      # parsed yaml after saving the settings
      java_opts <- self$
        compose$
        services$
        `ors-app`$
        environment[2]

      catalina_opts <- self$
        compose$
        services$
        `ors-app`$
        environment[3]

      # Put Java and Catalina options in quotes
      self$
        compose$
        services$
        `ors-app`$
        environment[2] <- shQuote(
          self$
          compose$
          services$
          `ors-app`$
          environment[2]
      )
      self$
        compose$
        services$
        `ors-app`$
        environment[3] <- shQuote(
          self$
          compose$
          services$
          `ors-app`$
          environment[3]
      )

      # Build yaml with indented bullet points
      yml_as_string <- yaml::as.yaml(
        self$compose,
        indent.mapping.sequence = TRUE
      )

      # Remove single quotes that are somehow added by as.yaml when introducing
      # double quotes.
       corrected_yml_string <- gsub("'\"|\"'", "\"", yml_as_string)

      # Remove line breaks of long strings
      corrected_yml_string <- corrected_yml_string %>%
        gsub("\\n\\s{8}-", " -", .)

      # Write new yaml to old yaml file
      cat(
        corrected_yml_string,
        file = "docker/docker-compose.yml"
      )

      self$
        compose$
        services$
        `ors-app`$
        environment[2] <- java_opts

      self$
        compose$
        services$
        `ors-app`$
        environment[3] <- catalina_opts
    }
  ),
  cloneable = FALSE
)
