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
  inherit = ORSInstance,
  active = list(

    #' @field graph_building Specifices whether the image is built for the
    #' first time or if the OSM extract is being changed. If `build` is
    #' assigned to the field, indiciates that the graphs are built for the
    #' first time. If `change` is assigned, indicates that the extract file
    #' was changed and the existing graphs should be overwritten using the new
    #' extract. If `NA` is assigned, indicates that no changes should be made
    #' and that graph building should not be forced.
    graph_building = function(mode) {
      build <- is.element(
        "build",
        names(
          self$
            compose$
            services$
            `ors-app`
        )
      )

      change <- !is.na(
        self$
          compose$
          services$
          `ors-app`$
          volumes[6]
      )

      gb <- self$compose$services$`ors-app`$environment[1] %>%
        strsplit("=") %>%
        unlist() %>%
        .[2] %>%
        as.logical()

      if(missing(mode)) {
        if(change) {
          if (gb) {
            return("change")
          } else {
            return(NA)
          }
        } else if(build) {
          return("build")
        } else {
          return(NA)
        }
      } else {
        if(is.null(pkg_cache$extract_path) && !is.na(mode)) {
          cli::cli_warn(
            paste(
              "Please set an extract before calling {.var $graph_building}"
            )
          )
          return(NA)
        }
        if (is.na(mode)) {
          private$.force_graphbuilding(handle = FALSE)
          self$save_compose()
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
                ORS_CONFIG = sprintf("./%s",
                                     "docker/data/ors-config.json"),
                OSM_FILE = sprintf("./%s",
                                   relativePath(pkg_cache$extract_path,
                                                super$dir))
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

          self$save_compose()
          return(mode)

        } else if (identical(mode, "change")) {
          private$.force_graphbuilding(handle = TRUE)
          change_node <- sprintf("./%s:/ors-core/data/osm_file.pbf",
                                 relativePath(pkg_cache$extract_path,
                                              file.path(super$dir, "docker")))

          self$
            compose$
            services$
            `ors-app`$
            volumes[6] <- change_node

          self$save_compose()
          return(mode)

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

    #' @field memory List of varius memory infos. Total and free memory refer
    #' to your system, init and max memory refer to the allocated memory of
    #' ORS.
    memory = list(
      total_memory = memuse::Sys.meminfo()$totalram@size,
      free_memory = memuse::Sys.meminfo()$freeram@size,
      init_memory = NULL,
      max_memory = NULL
    ),

    #' @description Initializes the `ORSSetupSettings` class. Reads the
    #' `docker-compose.yml` and adjusts the `Dockerfile`.
    initialize = function() {
      self$compose <- private$.read_dockercompose()
      private$.disable_auto_deletion()
      self$graph_building <- NA
      invisible(self)
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
      cli_abortifnot(is.null(init) || is.numeric(init))
      cli_abortifnot(is.null(max) || is.numeric(max))

      if (is.numeric(init) && is.numeric(max)) {
        private$.write_memory(init, max)
      } else if (is.numeric(init) && is.null(max)) {
        max <- init
        private$.write_memory(init, max)
      } else if (is.null(init) && is.numeric(max)) {
        init <- max / 2
        private$.write_memory(init, max)
      } else if (is.null(init) && is.null(max)) {
        if (
          !is.null(file.info(pkg_cache$extract_path)$size * 0.000001) &&
          !is.null(super$config$active_profiles)
        ) {
          max <- round(file.info(pkg_cache$extract_path)$size * 0.000001, -2) *
            2.5 *
            length(super$config$active_profiles) / 1000
          init <- max / 2
          private$.write_memory(init, max)
        } else {
          cli::cli_abort(
            c(
              paste("Set an extract and the active profiles or pass a",
                    "fixed amount of memory to be allocated."),
              paste("The memory estimation is based on the extract size and",
                    "the number of profiles.")
            )
          )
        }
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

      self$memory$init_memory <- init
      self$memory$max_memory <- max
      self$save_compose()
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
      file.open(file.path(super$dir, "docker/data/docker-compose.yml"))
    }
  ),

  private = list(

    .write_memory = function(init, max) {
      java_options <- self$compose$services$`ors-app`$environment[2]
      java_mem <- strsplit(java_options, " ") %>%
        unlist() %>%
        tail(2)
      init_mem_allocation <- java_mem[1]
      max_mem_allocation <- java_mem[2]
      java_options <- gsub(
        init_mem_allocation,
        sprintf("-Xms%sm", init * 1000),
        java_options
      )
      java_options <- gsub(
        max_mem_allocation,
        sprintf("-Xmx%sm", max * 1000),
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
      # Don't delete any profiles. Set up every profile at first start.
      dockerfile_path <- file.path(super$dir, "Dockerfile")

      dockerfile <- readLines(dockerfile_path, warn = FALSE)
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

      cat(dockerfile, file = dockerfile_path)
      }
    },

    .read_dockercompose = function() {
      yaml::read_yaml(file.path(super$dir, "docker/docker-compose.yml"))
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

      user <- self$
        compose$
        services$
        `ors-app`$
        user

      # Put string options in quadruple quotes
      self$
        compose$
        services$
        `ors-app`$
        environment[2] <- shQuote(
        self$
          compose$
          services$
          `ors-app`$
          environment[2],
        type = "cmd"
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
          environment[3],
        type = "cmd"
      )

      self$
        compose$
        services$
        `ors-app`$
        user <- shQuote(
        self$
          compose$
          services$
          `ors-app`$
          user,
        type = "cmd"
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
        file = file.path(super$dir, "docker/docker-compose.yml")
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

      self$
        compose$
        services$
        `ors-app`$
        user <- user
    }
  ),
  cloneable = FALSE
)
