#' ORS local instance
#' @description
#' Creates a new ORS instance object. This R6 class is typically
#' constructed by \code{\link{ors_instance}}.
#'
#' For technical details on the setup of local ORS instances, refer to the
#' \href{https://giscience.github.io/openrouteservice/run-instance/}{official ORS reference}.
#' For details on how to use \code{ORSLocal} objects, refer to the installation
#' vignette:
#'
#' \preformatted{
#' vignette("ors-installation", package = "rors")
#' }
#'
#'
#' @details
#' \code{ORSLocal} defines methods for all four steps in the ORS setup:
#' \itemize{
#'  \item{\bold{Extract: }}{Download an extract and set it up for graph building.}
#'  \item{\bold{Compose: }}{Change docker settings that control how the service is set up.}
#'  \item{\bold{Configuration: }}{Change the way ORS computes routes.}
#'  \item{\bold{Docker: }}{Send commands to docker to control the ORS container.}
#' }
#'
#' @section Docker commands:
#'
#' Local ORS instances are built using Docker containers. Initializing
#' \code{ORSLocal} downloads a \code{docker-compose.yml} file that tells
#' Docker how to build an ORS container. The Docker backend is pulled as
#' a Docker image (see
#' \href{https://hub.docker.com/r/openrouteservice/openrouteservice}{Dockerhub}).
#' \code{rors} communicates with Docker from within R to setup and manage ORS
#' containers.
#'
#' On Unix systems, Docker requires superuser permissions. In Unix shells,
#' this is not problematic. However, R cannot communicate with Docker without
#' explicitly being granted superuser permissions or at least permissions to
#' access Docker. Thus, \code{ORSLocal} checks if the current user is the
#' superuser or if the current user has access to the Docker group. If not,
#' it aborts. For more details on how to manage Docker as a non-root user on
#' Linux, refer to the
#' \href{https://docs.docker.com/engine/install/linux-postinstall/#manage-docker-as-a-non-root-user}{Docker documentation}.
#' Note that this procedure grants root-level privileges to a user and can
#' negatively impact your system's security.
#'
#' @section Jumpstarting:
#'
#' When first initializing \code{ORSLocal} or when first creating a new
#' directory, \code{ORSLocal} will ask you if you would like to perform a
#' jumpstart. Jumpstarting means creating an initial Docker build that runs
#' on default settings, config and an extract file from Heidelberg, Germany.
#' On the initial build, Docker also creates the file system of ORS instances
#' including a configuration file. Irrespective of your choice to jumpstart,
#' Docker only allows one routing profile and automatically uses the
#' default extract of Heidelberg on the initial run.
#'
#' By jumpstarting, \code{ORSLocal} takes over this initial build,
#' so that subsequent builds run flawlessly without the need to manually
#' create the file system. This is deemed good practice by the ORS developer
#' team (refer to the
#' \href{https://giscience.github.io/openrouteservice/run-instance/installation/running-with-docker}{ORS reference}
#' for details).
#'
#' Conversely, if you choose not to jumpstart (\code{dry = TRUE}),
#' \code{ORSLocal} does not interact with Docker until explicitly told to.
#' The configuration and extract files as well as the rest of the file system
#' do not exist yet. You can manually jumpstart by calling \code{$up()}.
#' Initializing a dry instance can be useful if you need to make changes to
#' the compose file before building ORS for the first time.
#'
#' @section Manual changes:
#'
#' \code{ORSLocal} provides a range of convenience methods to change the
#' setup of the ORS instance. All of these methods automatically read or write
#' their changes to disk. It is also possible to make direct changes to the
#' (parsed) configuration and compose files. In this case, reading and writing
#' also need to be done manually. To write changes to disk, run
#' \code{$update()}, e.g.:
#'
#' \preformatted{
#'  ors$compose$parsed$services$`ors-app`$container_name <- "new-name-123"
#'  ors$update()
#' }
#'
#' To read changes done manually to the files on disk, run
#' \code{$update("self")}.
#'
#'
#' @examples
#' \dontrun{
#' # Download ORS, start docker and jumpstart a default session
#' ors <- ors_instance("~/ex_ors", version = "7.2.0")
#'
#' # Take down the newly created instance
#' ors$down()
#'
#' # Set a new extract file
#' ors$set_extract("Rutland")
#'
#' # Allocate 100 MB of RAM
#' ors$set_ram(0.1)
#'
#' # Add a routing profile
#' walking <- ors_profile("walking")
#' ors$add_profiles(walking)
#'
#' # Set random port
#' ors$set_port()
#'
#' # Change project name
#' ors$set_name("example-ors")
#'
#' # Set up ORS
#' ors$up()
#'
#' # Check if ORS container exists and is running
#' ors$is_built()
#' ors$is_running()
#'
#' # Check if ORS is ready to use
#' ors$is_ready()
#'
#' # Stop container, e.g. to make configuration changes
#' ors$stop()
#'
#' # Make changes to the configuration
#' ors$set_endpoints(matrix = list(maximum_routes = 1e+05)) # default is 2500
#'
#' # Change default profile settings
#' default <- ors_profile(maximum_snapping_radius = -1)
#' ors$add_profiles(default)
#'
#' # If we make manual changes to the configuration, we need to apply the
#' # changes explicitly
#' ors$config$parsed$ors$engine$profiles$car$elevation <- FALSE
#' ors$update() # writes the current object state to the disk
#'
#' # If the compose or config files are changed on disk, the object can be
#' # refreshed
#' ors$update("self") # reads the disk state to the R object
#'
#' # Adding profiles does not work when the container is still built!
#' # Why? Because graphs need to be built for new profiles, so the container
#' # must be down.
#' if (FALSE) {
#'   bike <- ors_profile("bike-road")
#'   ors$add_profiles(bike) # only do this when the container is down!!
#' }
#'
#' # Additionally, graphs are only re-built if we enable graph building.
#' # When changing the extract, this happens automatically, but we can also
#' # control graph building manually, e.g. for adding new profiles.
#' ors$set_graphbuilding(TRUE)
#'
#' # Finally, start the container again to run the newly configured service
#' ors$start()
#' }
#'
#'
#' @export
ORSLocal <- R6::R6Class(
  classname = "ORSLocal",
  inherit = ORSInstance,

  # Public ----
  public = list(
    #' @field paths List of relevant file paths for the ORS setup. Includes
    #' the top directory, compose file, config file and extract file.
    paths = list(),

    #' @field version Version of the local ORS backend
    version = NULL,

    #' @field compose Information of the compose file (\code{docker-compose.yml}).
    #' The compose file holds various settings for the Docker setup. The most
    #' important settings are included in this field:
    #'
    #' \itemize{
    #'  \item{\code{ports}: A 2×2 matrix with Docker ports}
    #'  \item{\code{name}: Name of the Docker container}
    #'  \item{\code{memory}: List with memory information on total and free
    #'  system memory as well as initial and max memory allocated to the Docker
    #'  instance.}
    #'  \item{\code{image}: Version of the ORS image. \code{"latest"} refers
    #'  to the latest stable version. \code{"nightly"} refers to the devel
    #'  version.}
    #'  \item{\code{parsed}: Parsed compose file. When making changes to this
    #'  object, make sure to run \code{$update()} to apply the changes. For details,
    #' refer to the \href{https://giscience.github.io/openrouteservice/run-instance/installation/running-with-docker#docker-configuration}{official reference}.}
    #' }
    compose = NULL,

    #' @field config Information of the configuration file (\code{ors-config.yml}).
    #' The config file holds various options about the ORS instance. This
    #' field gives details about:
    #'
    #' \itemize{
    #'  \item{profiles: A named vector of active routing profiles}
    #'  \item{parsed: Parsed configuration file. When making changes to
    #'  this obhect, make sure to run \code{$update()} to apply the changes.
    #'  For details, refer to the
    #'  \href{https://giscience.github.io/openrouteservice/run-instance/configuration/}{ORS reference}.}
    #' }
    config = NULL,

    #' @field extract Information on the extract file. Contains the name and
    #' size of the selected extract file.
    extract = NULL,

    ## Meta ----
    #' @description
    #' Initialize the ORSInstance object.
    #'
    #' @param dir \code{[character]}
    #'
    #' Custom OpenRouteService directory. If not specified, the directory
    #' will be downloaded to the system's home directory. If a directory called
    #' \code{"openrouteservice-{version}"} is present, the download will be skipped.
    #' Ignored if \code{server} is not \code{NULL}.
    #' @param version \code{[character]}
    #'
    #' The OpenRouteService version to use. Can either be a version number (e.g.
    #' 7.2.0) or \code{"master"}. Ignored if \code{server} is not \code{NULL}.
    #' @param overwrite \code{[logical]}
    #'
    #' Whether to overwrite the current OpenRouteService directory
    #' if it exists.
    #' @param dry \code{[logical]}
    #'
    #' Whether to start a dry run, i.e. run an instance without jumpstarting.
    #' @param verbose \code{[logical]}
    #'
    #' Level of verbosity. If \code{TRUE}, shows informative warnings and messages,
    #' spinners, progress bars and system notifications.
    #' @param prompts \code{[logical]}
    #'
    #' Whether to ask for permission throughout the setup. Defaults to
    #' \code{TRUE} in interactive sessions.
    initialize = function(dir = ".",
                          version = "7.2.0",
                          overwrite = FALSE,
                          dry = FALSE,
                          verbose = TRUE,
                          prompts = interactive()) {
      assert_that(assertthat::is.dir(dir), add = paste(
        "The {.var dir} argument is expected to be a valid path to store",
        "the OpenRouteService source code in."
      ))

      assert_that(
        assertthat::is.string(version),
        is_true_or_false(overwrite),
        is_true_or_false(dry),
        is_true_or_false(verbose),
        is_true_or_false(prompts)
      )

      if (!dry) {
        check_docker_installation()
        check_docker_access()
        start_docker(verbose = verbose)
      }

      dir <- get_ors_release(dir, version, overwrite)

      private$.verbose <- verbose
      private$.prompts <- prompts
      private$.overwrite <- overwrite
      private$.dry <- dry
      self$version <- version
      self$paths$top <- dir
      private$.parse()

      # if possible, re-use version as image version
      if (is_numver(version) || is_version_desc(version)) {
        if (is_version_desc(version, "gh")) {
          version <- "nightly"
        }

        self$compose$parsed <- set_compose_image(self$compose$parsed, version)
        private$.write()
      }

      if (private$.is_initial() && !dry) {
        private$.jumpstart()
      }

      private$.mount()
      invisible(self)
    },

    #' @description
    #' Updates ORS instance. Use this to apply changes made either in the
    #' file system or to the \code{\link{ors_instance}} object itself. This
    #' method is automatically called when using any method of
    #' \code{ors_instance} that changes the ORS setup.
    #'
    #' @param what \code{[character]}
    #'
    #' Whether to change the file system with changes in R or update the
    #' instance object with changes in the file system. If \code{what = "self"},
    #' parses the relevant files in the ORS directory and updates the
    #' \code{ors_instance} object. If \code{what = "fs"}, updates the
    #' compose file and config file based on the changes made to the
    #' \code{ors_instance} object.
    update = function(what = c("fs", "self")) {
      what <- match.arg(what)
      switch(what, fs = private$.write(), self = private$.parse())
      private$.mount()
      invisible(self)
    },

    #' @description
    #' Purge ORS instance, i.e. take down container, (optionally) delete
    #' image, delete ORS directory, and clean up R6 class.
    #'
    #' This method can be useful for testing and writing reproducible
    #' examples and can easily be used together with \code{\link{on.exit}}.
    #'
    #' @param image \code{[logical]}
    #'
    #' Whether to remove the docker image or keep it for other projects. The
    #' default is \code{FALSE} to prevent accidentally breaking other projects.
    purge = function(image = FALSE) {
      if (!private$.alive) {
        return(invisible(NULL))
      }

      if (private$.prompts) {
        ors_cli(info = list(c("i" = paste(
          "Purging the current instance removes the docker container,",
          "ORS directory and cleans up the R6 object."
        ))))

        yes_no("Do you want to proceed?", no = cancel())
      }

      ors_cli(h2 = "Purging ORS")

      if (self$is_built()) self$down()
      if (image) rm_image(self, private)

      ors_cli(progress = list(
        "step",
        msg = "Removing ORS directory",
        msg_done = "Removed ORS directory",
        msg_failed = "Could not remove ORS directory"
      ))

      unlink(self$paths$top, recursive = TRUE, force = TRUE)

      ors_cli(progress = list(
        "step",
        msg = "Cleaning up R6 object",
        msg_done = "Cleaned up R6 object",
        msg_failed = "Could not clean up R6 object"
      ))

      self$extract <- NULL
      self$compose <- NULL
      self$config <- NULL
      self$paths <- NULL
      private$.alive <- FALSE
      invisible(NULL)
    },

    #' @description
    #' Prints a situation report of the ORS instance. Invokes all relevant
    #' print methods that summarize the current state of the instance
    #' object.
    report = function() {
      print(self)

      if (!is.null(self$compose)) {
        cli::cat_line()
        print(self$compose)
      }

      if (!is.null(self$extract)) {
        cli::cat_line()
        print(self$extract)
      }

      if (!is.null(self$config)) {
        cli::cat_line()
        print(self$config)
      }

      invisible(self)
    },


    ## Extract ----
    #' @description
    #' Download and set an OpenStreetMap extract for use in ORS. Wrapper for
    #' \code{\link[osmextract:oe_get]{oe_get()}}.
    #'
    #' @param place \code{[various]}
    #'
    #' Place name, \code{sf/sfc/bbox} object or bounding box for which to
    #' download an extract file. For details, refer to
    #' \code{\link[osmextract:oe_match]{oe_match()}}.
    #' @param provider \code{[character/NULL]}
    #'
    #' Extract provider to download extract from. Available
    #' providers can be found by running
    #' \code{\link[osmextract:oe_providers]{oe_providers()}}. If \code{NULL},
    #' tries all providers.
    #'
    #' @param timeout \code{[numeric]}
    #'
    #' Timeout for extract downloads. Defaults to 30 minutes to enable
    #' longer extract downloads. The adopted timeout is the maximum of
    #' this argument and \code{getOption("timeout")}.
    #' @param file \code{[character/NULL]}
    #'
    #' Path to a local OSM extract. Can either be a full path to any OSM file
    #' or the filename of an OSM file in the data folder of ORS. If file is
    #' specified, \code{place} and \code{provider} are ignored.
    #' @param do_use \code{[logical]}
    #'
    #' If \code{TRUE}, enables graph building with the new extract. If
    #' \code{FALSE}, does not change the compose file at all.
    #' @param ... Further arguments passed to
    #' \code{\link[osmextract:oe_get]{oe_get()}}.
    set_extract = function(place,
                           provider = "geofabrik",
                           timeout = 1800,
                           file = NULL,
                           do_use = TRUE,
                           ...) {
      if (is.null(file)) {
        file <- get_extract(
          self,
          private,
          place = place,
          provider = provider,
          timeout = timeout,
          ...
        )
      } else {
        file <- set_extract(self, file)
      }

      if (do_use && !is.null(file)) {
        ors_cli(info = list(c("*" = "Using extract {.val {basename(file)}}")))
        self$paths$extract <- file
        self$extract <- list(
          name = basename(file),
          size = round(file.size(file) / 1024 / 1024, 2)
        )
        self$set_graphbuilding(TRUE)
        self$compose$parsed <- change_extract(self$compose$parsed, file)
      }

      self$update()
    },

    #' @description
    #' Removes extract files from the data directory.
    #'
    #' @param ... File names of extract files in the data directory. All
    #' files that exist are removed. Can also be a single vector of file
    #' names.
    rm_extract = function(...) {
      old <- c(...)

      if (is.null(old)) {
        all_files <- list.files(file.path(self$paths$top, "docker", "data"))
        old <- all_files[is_pbf(all_files)]
      }

      old <- file.path(self$paths$top, "docker", "data", old)
      changed <- vapply(old, file.exists, logical(1))

      if (any(changed)) {
        to_rm <- old[changed]
        cur_extract <- identify_extract(self$compose$parsed, self$paths$top)
        to_rm_fmt <- basename(to_rm)
        to_rm_fmt[to_rm %in% cur_extract] <- paste(
          basename(cur_extract), cli::col_red("<- active extract")
        )
        ors_cli(info = list(c("*" = "Removing extract files:")))
        ors_cli(bullets = list(stats::setNames(
          sprintf("- %s", to_rm_fmt), rep(" ", length(to_rm))
        )))

        yes_no(
          "Do you want to proceed?",
          no = cancel(),
          ask = private$.prompts
        )

        for (extract in to_rm) {
          unlink(extract, recursive = FALSE)
        }

        if (cur_extract %in% to_rm) {
          ors_cli(info = list(c(
            "*" = "Unsetting active extract file: {.emph {basename(cur_extract)}}"
          )))
          self$extract <- NULL
          self$compose$parsed <- change_extract(self$compose$parsed, NULL)
          self$update()
        }
      }
    },


    ## Compose ----
    #' @description
    #' Set a name for the ORS container.
    #'
    #' @param name Name for the ORS container. If \code{NULL}, generates
    #' a random name (\code{"ors-appXXXX"}).
    set_name = function(name = NULL) {
      old <- self$compose$parsed$services$`ors-app`$container_name
      new <- name %||% random_ors_name(private, name)

      if (!identical(old, new)) {
        ors_cli(info = list(c("*" = "Setting name to {.val {new}}")))
        self$compose$parsed$services$`ors-app`$container_name <- new
        self$compose$name <- new
        self$update()
      }
    },

    #' @description
    #' Set a port for the localhost of the ORS container.
    #'
    #' @param port Port to use for the container. If \code{NULL}, assigns a
    #' random port using
    #' \code{\link[httpuv:randomPort]{randomPort()}}.
    set_port = function(port = NULL) {
      new <- as.character(port %||% random_port(2))
      old <- self$compose$ports[1, seq(length(new))]


      if (!identical(old, new)) {
        ors_cli(info = list(c(
          "*" = "Setting {cli::qty(length(new))} port{?s} to {.val {new}}"
        )))
        compose <- self$compose$parsed
        self$compose$parsed$services$`ors-app`$ports <- format_ports(self, new)
        self$compose$ports[1, seq(length(new))] <- new
        self$update()
      }
    },

    #' @description
    #' Set initial and max memory that the ORS container is allowed to use.
    #'
    #' @param init \code{[numeric/NULL]}
    #'
    #' Initial memory. This can change if more memory is needed. If not
    #' specified, uses \code{max}. If both are \code{NULL}, estimates
    #' memory.
    #' @param max \code{[numeric/NULL]}
    #'
    #' Maximum memory. The container is not allowed to use more
    #' memory than this value. If not specified, uses \code{init}. If both are
    #' \code{NULL}, estimates memory.
    set_ram = function(init = NULL, max = NULL) {
      old <- unlist(self$compose$memory[3:4], use.names = FALSE) * 1000
      new <- adjust_memory(self, private, init, max)

      if (!identical(old, new) && !is.null(new)) {
        ors_cli(info = list(c("*" = "Setting memory to:")))
        ors_cli(bullets = list(stats::setNames(
          paste0(cli::style_bold(sprintf(
            c("- init: {.field {%s}} GB", "- max: {.field {%s}} GB"),
            new / 1000
          ))),
          rep(" ", 2)
        )))
        self$compose$parsed$services$`ors-app`$environment[2] <- format_memory(self, new)
        self$compose$memory$init <- new[1] / 1000
        self$compose$memory$max <- new[2] / 1000
        self$update()
      }
    },

    #' @description
    #' Graph building specifies whether routing graphs should be (re-)built.
    #' Turning graph building on enables new profiles to be built or the
    #' extract to be changed but significantly increases setup time. Turn
    #' this off if you are changing configuration options that do not alter
    #' routing graphs.
    #'
    #' @param mode \code{[logical]}
    #'
    #' Whether to turn graph building on or off.
    set_graphbuilding = function(mode) {
      old <- self$compose$graph_building
      new <- mode

      if (!identical(old, new)) {
        verb <- ifelse(mode, "Enabling", "Disabling")
        ors_cli(info = list(c("*" = "{verb} graph building")))
        self$compose$parsed$services$`ors-app`$environment[1] <- set_gp(self, mode)
        self$compose$graph_building <- mode
        self$update()
      }
    },

    #' @description
    #' Set version of the ORS Docker image. This should preferably be compatible
    #' with the compose version.
    #'
    #' @param version \code{[character]}
    #'
    #' Version specification of the ORS image.
    set_image = function(version = NULL) {
      old <- self$compose$version
      new <- check_version(version) %||% old

      if (!identical(old, new)) {
        ors_cli(info = list(c("*" = "Setting image version to {.field {new}}")))
        self$compose$parsed$services$`ors-app`$image <- paste0(
          "openrouteservice/openrouteservice:", new
        )
        self$compose$version <- new
        self$update()
      }
    },


    ## Config ----
    #' @description
    #' Add routing profiles to the ORS configuration. ORS only builds routing
    #' graphs for active profiles.
    #'
    #' @param ... Objects of class \code{\link{ors_profile}} or character
    #' strings. If a character string is passed, it is interpreted as
    #' \code{ors_profile(..., template = TRUE)}.
    add_profiles = function(...) {
      old <- self$config$parsed$ors$engine
      new <- insert_profiles(self, private, ...)
      changed <- compare_config(old$profiles, new$profiles)
      changed_dflt <- "profile_default" %in% names(new) &&
        !equivalent_list(old$profile_default, new$profile_default)

      if (length(changed) || changed_dflt) {
        if (length(changed)) {
          ors_cli(info = list(c(
            "*" = "Adding {cli::qty(changed)} profile{?s}: {.field {changed}}"
          )))
        }

        if (changed_dflt) {
          ors_cli(info = list(c(
            "*" = "Adding profile defaults"
          )))
        }

        self$config$parsed$ors$engine <- new
        self$config$profiles <- names(new$profiles)
        self$update()
      }
    },

    #' @description
    #' Remove routing profiles from the ORS configuration.
    #'
    #' @param ... Names of routing profiles to remove. \code{"default"}
    #' removes profile defaults. Can also be a single character vector.
    rm_profiles = function(...) {
      old <- self$config$profiles
      new <- c(...)
      changed <- setdiff(new, "default") %in% c(old, names(old))
      changed_dflt <- "profile_default" %in% names(self$config$parsed$ors$engine) &&
        "default" %in% new

      if (any(changed) || changed_dflt) {
        if (any(changed)) {
          ors_cli(info = list(c("*" = paste(
            "Removing {cli::qty(length(new[changed]))}",
            "profile{?s}: {.field {new[changed]}}"
          ))))
        }

        if (changed_dflt) {
          ors_cli(info = list(c("*" = "Removing profile defaults")))
        }

        self$config$parsed$ors$engine <- remove_profiles(self, ...)
        self$config$profiles <- get_profile_names(
          self$config$parsed$ors$engine$profiles
        )
        self$update()
      }
    },

    #' @description
    #' Change endpoint-specific configurations. Specifies options that are
    #' relevant for entire API endpoints such as isochrones.
    #'
    #' @param ... \code{[list/NULL]}
    #'
    #' Named arguments containing the configuration for the endpoints.
    #' Available endpoits are \code{routing}, \code{isochrones},
    #' \code{matrix}, and \code{snap}. Refer to
    #' \href{https://github.com/GIScience/openrouteservice/blob/master/ors-api/src/main/resources/application.yml}{application.yml}
    #' for a list of defaults.
    set_endpoints = function(...) {
      old <- self$config$parsed$ors$endpoints
      new <- list(...)
      changed <- compare_endpoints(old, new)

      if (length(changed)) {
        verb <- ifelse(is.null(old), "Adding", "Changing")
        ors_cli(info = list(c("*" = "{verb} the following endpoints:")))
        ors_cli(bullets = list(stats::setNames(
          cli::style_bold(paste("-", changed)),
          rep(" ", length(changed))
        )))
        self$config$parsed$ors$endpoints <- change_endpoints(self, ...)
        self$update()
      }
    },


    ## Docker ----
    #' @description
    #' Create the ORS docker container and setup the ORS backend on a local
    #' host.
    #'
    #' @param wait \code{logical}
    #'
    #' Whether to run a spinner and show a system notification when the setup
    #' is completed. If \code{FALSE} releases the console after the Docker
    #' container is created. You can then check the service status using
    #' \code{$is_ready()}.
    #'
    #' @param ... Additional flags passed to the \code{docker up} command.
    #'
    #' @details
    #' The exact command run by \code{$up()} is:
    #'
    #' \code{docker compose -p [name] -f [compose file] up -d --no-build [...]}
    #'
    #' The \code{-p} flag allows docker to run multiple ORS containers and keep
    #' them separate. It uses the service name defined in the compose file.
    #'
    #' If not found, \code{$up()} builds the underlying OpenRouteService docker
    #' image specified by \code{version} during the initialization of
    #' \code{ORSLocal}.
    #'
    #' Usually in detach mode (\code{-d}), docker returns terminal control back
    #' to the user. By default, \code{$up()} blocks the console, checks for
    #' errors and notifies the user when the service setup has finished. This
    #' behavior can be suppresed by setting \code{wait = FALSE}. The service
    #' status can then be checked using \code{$is_ready()} or
    #' \code{\link[=ors_ready]{ors_ready()}}. Container logs can be accessed by typing
    #' \code{docker logs [name]} in the terminal.
    up = function(wait = TRUE, ...) {
      ors_up(self, private, wait, ...)
      self$set_graphbuilding(FALSE)
      private$.mount()
      invisible(self)
    },

    #' @description
    #' Stop and remove the ORS docker container. Use this if you want to make
    #' changes to a running ORS setup such as changing the extract or selected
    #' profiles.
    down = function() {
      ors_down(self, private)
      private$.mount()
      invisible(self)
    },

    #' @description
    #' Start the ORS docker container.
    #'
    #' @param wait \code{logical}
    #'
    #' Whether to run a spinner and show a system notification when the setup
    #' is completed. If \code{FALSE} releases the console after the Docker
    #' container is created. You can then check the service status using
    #' \code{$is_ready()}.
    start = function(wait = TRUE) {
      ors_start(self, private, wait)
      private$.mount()
      invisible(self)
    },

    #' @description
    #' Stop the ORS docker container.
    stop = function() {
      ors_stop(self, private)
      private$.mount()
      invisible(self)
    },

    #' @description
    #' Retrieve technical information on the docker image used.
    get_image = function() {
      image <- self$get_container()$image
      container_image(image)
    },

    #' @description
    #' Retrieve technical information on the docker container used.
    get_container = function() {
      container_info(self$compose$name)
    },

    #' @description
    #' Show container logs as returned by \code{docker logs}. Useful for
    #' debugging docker setups.
    #'
    #' @param format \code{[logical]}
    #'
    #' If \code{TRUE}, includes ANSI colors and adds exdents. Otherwise,
    #' trims ANSI colors. Disabling formatting increases performance, which
    #' can be useful for larger logs.
    show_logs = function(format = TRUE) {
      logs <- docker_logs(self$compose$name)

      if (format) {
        logs <- strwrap(logs, width = cli::console_width(), exdent = 2)
        logs <- gsub("\\s", "\u00a0", logs)
      } else {
        logs <- cli::ansi_strip(logs)
      }

      cli::cli_text(paste(logs, collapse = "\f"))
      invisible(logs)
    },

    #' @description
    #' Checks if the ORS container is built. You can control this state
    #' using \code{$up()} and \code{$down()}.
    is_built = function() {
      container_built(self$compose$name)
    },

    #' @description
    #' Checks if the ORS container is running. You can control this state
    #' using \code{$start()} and \code{$stop()}. Check \code{$is_ready()} to see
    #' if the ORS setup succeeded.
    is_running = function() {
      container_running(self$compose$name)
    }
  ),

  # Private ----
  private = list(
    .overwrite = FALSE,
    .verbose = TRUE,
    .prompts = TRUE,
    .dry = FALSE,
    .alive = TRUE,
    .mount = function() {
      assign("instance", self, envir = ors_cache)
    },
    .parse = function() {
      self$paths <- private$.construct("paths")
      self$compose <- private$.construct("compose")
      self$extract <- private$.construct("extract")
      self$config <- private$.construct("config")
    },
    .write = function() {
      if (!is.null(self$compose))
        write_dockercompose(self$compose$parsed, self$paths$compose)

      if (!is.null(self$config))
        write_config(self$config$parsed, self$paths$config)
    },
    .construct = function(what, ...) {
      construct <- get0(paste0(".construct_", what))

      if (is.function(construct)) {
        construct(self, private, ...)
      }
    },
    .is_initial = function() {
      # Assume existence of graphs dir as indicator for initialization
      # TODO: Could there be a better indicator?
      !dir.exists(file.path(self$paths$top, "docker", "graphs"))
    },
    .jumpstart = function() {
      ors_cli(h1 = "Jumpstarting container")

      if (private$.verbose) {
        ors_cli(
          cat = "line",
          info = list(c("i" = paste(
            "Docker will now jumpstart an initial ORS setup. This setup runs the",
            "default config, settings, and data from Heidelberg, Germany.",
            "It will fail for any coordinates outside of Heidelberg.",
            "You can make changes to this setup afterwards.",
            "This process can take a few minutes.",
            "Jumpstarting is recommended by the ORS developer team, but you can",
            "also start a dry run and jumpstart yourself."
          )))
        )
      } else {
        cli::cli_inform(list(c(
          "i" = "Docker will now jumpstart an initial ORS setup.")
        ))
      }

      proceed <- yes_no(
        "Do you want to proceed?",
        ask = private$.prompts,
        dflt = TRUE
      )
      if (isFALSE(proceed)) return()

      # For initialization, avoid overwriting existing setups by setting
      # a random container name and port
      self$set_name()
      self$set_port()

      self$up()
      self$update("self")

      ors_cli(
        cat = "line",
        info = list(c("i" = paste(
          "If you wish to make changes to the existing setup,",
          "run {.code $down()}, or {.code $stop()} and make the",
          "required changes manually."
        )))
      )
    }
  )
)



# External ----

.construct_paths <- function(self, private) {
  # get all easily available paths
  dir <- self$paths$top
  compose_path <- file.path(dir, "docker-compose.yml")

  # try to find config, otherwise null
  config_path <- detect_config(dir)

  # try to find extract path, otherwise null
  compose <- read_ors_yaml(compose_path)
  extract_path <- get_current_extract(compose, dir)

  # construct R representation
  structure(
    list(
      top = dir,
      extract = extract_path,
      config = config_path,
      compose = compose_path
    ),
    class = "ors_paths"
  )
}

.construct_compose <- function(self, private, ...) {
  # try to read compose file
  if (!is.null(self$paths$compose)) {
    compose <- read_ors_yaml(self$paths$compose)
  } else {
    return(NULL)
  }

  # parse compose contents
  available_mem <- get_memory_info()
  name <- compose$services$`ors-app`$container_name
  ports <- ports_to_df(compose$services$`ors-app`$ports)
  memory <- read_memory(compose, num = TRUE)
  memory <- c(available_mem, memory)
  image <- read_compose_image(compose)
  graph_building <- graphbuilding_enabled(compose)

  # construct R representation
  structure(
    list(
      name = name,
      ports = ports,
      memory = memory,
      image = image,
      graph_building = graph_building,
      parsed = structure(compose, class = c("ors_compose_parsed", "list"))
    ),
    class = "ors_settings",
    update_compose = NULL
  )
}


.construct_config <- function(self, private, ...) {
  # try to read config file
  if (!is.null(self$paths$config)) {
    config <- read_ors_yaml(self$paths$config)
  } else {
    return(NULL)
  }

  # parse the most important config contents
  profiles <- get_profile_names(config$ors$engine$profiles)

  # construct R representation
  structure(
    list(
      profiles = profiles,
      parsed = structure(config, class = c("ors_config_parsed", "list"))
    ),
    class = "ors_config"
  )
}


.construct_extract <- function(self, private, ...) {
  if (is.null(self$paths$extract)) {
    return(NULL)
  }

  # parse extract information
  extract_path <- self$paths$extract
  if (!is.null(extract_path)) {
    name <- basename(extract_path)
    size <- round(file.size(extract_path) / 1024 / 1024, 0)
  } else {
    name <- size <- NULL
  }

  # construct R representation
  structure(list(name = name, size = size), class = "ors_extract")
}


get_ors_release <- function(dir, version, overwrite) {
  dir <- normalizePath(dir, winslash = "/")
  dir <- file.path(dir, sprintf("openrouteservice-%s", version))

  if (!dir.exists(dir) || overwrite) {
    dir.create(dir, recursive = TRUE)

    url <- "https://raw.githubusercontent.com/GIScience/openrouteservice/%s/docker-compose.yml"

    if (is_version_desc(version, "dh")) {
      version <- "master"
    }

    if (is_numver(version)) {
      version <- paste0("v", version)
    }

    url <- sprintf(url, version)
    compose <- tryCatch(
      read_ors_yaml(url),
      warning = function(w) {
        if (grepl("404 Not Found", w$message, fixed = TRUE)) {
          unlink(dir, recursive = TRUE)
          cli::cli_abort("ORS version/commit {.val {version}} does not exist.")
        } else {
          cli::cli_warn(w$message)
        }
      }
    )

    write_dockercompose(compose, file = file.path(dir, "docker-compose.yml"))
  }

  dir
}


start_docker <- function(verbose = TRUE) {
  if (docker_running()) {
    return(invisible(NULL))
  }

  if (is_windows()) {
    ors_cli(info = list(c(
      "i" = paste(
        "Docker is required to be running in order to",
        "start an OpenRouteService instance."
      )
    )))

    docker_path <- Sys.which("docker")
    docker_desktop <- file.path(
      file_path_up(docker_path, 3L),
      "Docker Desktop.exe"
    )

    status <- file.open(docker_desktop)

    # If Docker is installed, it will try to open
    if (status() == 0L || is.null(status())) {
      ors_cli(progress = list(
        "step",
        msg = "Starting Docker...",
        spinner = verbose,
        msg_done = "Docker Desktop is now running.",
        msg_failed = "The Docker startup has timed out."
      ))

      # Check if Docker is usable by running a Docker command
      proc <- callr::r_bg(
        function(docker_running) {
          while (!docker_running()) {
            Sys.sleep(1L)
          }
        },
        package = TRUE,
        args = list(docker_running)
      )

      while (proc$is_alive()) {
        ors_cli(progress = "update")
        Sys.sleep(0.01)
        difft <- difftime(Sys.time(), proc$get_start_time(), units = "secs")
        if (difft > 180L) cli::cli_abort("Docker startup timed out.")
      }
    } else {
      cli::cli_abort(
        "Something went wrong while starting Docker. Is it installed?"
      )
    }
  } else if (is_linux()) {
    callr::run(
      command = "systemctl",
      args = c("start", "docker"),
      stdout = NULL,
      stderr = NULL
    )
  }
}


read_ors_yaml <- function(path, ...) {
  if (is.null(path)) {
    cli::cli_abort(c(
      "!" = "Could not find {basename(path)}.",
      "i" = "Is your ORS directory properly initialized?"
    ))
  }

  yaml::read_yaml(path, ..., readLines.warn = FALSE)
}
