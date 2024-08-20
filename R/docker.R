#' ORS Docker instance
#' @description
#' Creates a new ORS instance object that operates on a Docker instance of
#' OpenRouteService. This R6 class is typically constructed by
#' \code{\link{ors_instance}}. \code{ORSDocker} requires Docker to be
#' installed on the system and accessible by the current user. If these
#' requirements cannot be met, consider using \code{\link{ORSJar}} or
#' \code{\link{ORSWar}}.
#'
#' For technical details on the setup of local ORS instances, refer to the
#' \href{https://giscience.github.io/openrouteservice/run-instance/running-with-docker}{Running with Docker documentation}.
#' For details on how to use \code{ORSDocker} objects, refer to the installation
#' vignette:
#'
#' \preformatted{
#' vignette("ors-installation", package = "rors")
#' }
#'
#'
#' @details
#' \code{ORSDocker} defines methods for all four steps in the ORS setup:
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
#' @section Configuration files:
#'
#' In line with the
#' \href{https://giscience.github.io/openrouteservice/run-instance/configuration/}{ORS documentation},
#' \code{ORSLocal} searches for a pre-defined configuration file in the
#' following places (in this order):
#'
#' \itemize{
#'  \item{Config directory: \code{./docker/config/ors-config.yml}}
#'  \item{R working directory: \code{{getwd()}/ors-config.yml}}
#'  \item{ORS runtime directory: \code{./ors-config.yml}}
#'  \item{User directory: \code{~/.openrouteservice/ors-config.yml} (Linux only)}
#'  \item{Global directory: \code{/etc/openrouteservice/ors-config.yml} (Linux only)}
#' }
#'
#' If none of these files is found, \code{ORSLocal} checks for the existence
#' of a \code{ors-config.json} file and - if found - invokes a warning.
#' If no configuration file is found at all, a new minimal configuration is
#' created containing only a source file specification and one profile (car).
#' Alternative configuration files can be specified by modifying the
#' \code{ORS_CONFIG_LOCATION} option in the compose file:
#'
#' \preformatted{
#' ors$compose$parsed$services$`ors-app`$environment$ORS_CONFIG_LOCATION <- "ors-config.yml"
#' ors$update()
#' }
#'
#' @examples
#' \dontrun{
#' # Download ORS, start docker and jumpstart a default session
#' ors <- ors_instance("~/ex_ors")
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
ORSDocker <- R6::R6Class(
  classname = "ORSDocker",
  inherit = ORSLocal,

  # Public ----
  public = list(
    #' @field paths List of relevant file paths for the ORS setup. Includes
    #' the top directory, compose file, config file, and extract file.
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
    #' Initialize the ORSDocker object.
    #'
    #' @param dir \code{[character]}
    #'
    #' Custom OpenRouteService directory. If not specified, the compose file
    #' will be downloaded to the current working directory. If a directory called
    #' \code{"openrouteservice-{version}"} is present, the download will be skipped.
    #' Ignored if \code{server} is not \code{NULL}.
    #' @param version \code{[character]}
    #'
    #' The OpenRouteService version to use. Can either be a version number (e.g.
    #' 8.1.1) or \code{"master"}.
    #' @param overwrite \code{[logical]}
    #'
    #' Whether to overwrite the current OpenRouteService directory if it exists.
    #' @param dry \code{[logical]}
    #'
    #' Whether to start a dry run, i.e. initialize a docker instance without
    #' requiring docker. This allows you to manipulate config, compose file,
    #' and extract, but does not allow you to interact with docker.
    #' @param verbose \code{[logical]}
    #'
    #' Level of verbosity. If \code{TRUE}, shows informative warnings and messages,
    #' spinners, progress bars and system notifications.
    #' @param prompts \code{[logical]}
    #'
    #' Whether to ask for permission throughout the setup. Defaults to
    #' \code{TRUE} in interactive sessions.
    #' @param ... Not used.
    initialize = function(dir = ".",
                          version = "8.1.1",
                          overwrite = FALSE,
                          dry = FALSE,
                          verbose = TRUE,
                          prompts = interactive(),
                          ...) {
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

      dir <- get_ors_release(dir, version, file = "docker", overwrite, verbose)
      check_ors_dir(dir, type = "docker")

      private$.verbose <- verbose
      private$.prompts <- prompts
      private$.overwrite <- overwrite
      self$version <- version
      self$paths$top <- dir
      private$.parse()

      # create directories manually
      create_ors_docker_dirs(dir)

      # if possible, re-use version as image version
      if (is_numver(version) || is_version_desc(version)) {
        if (is_version_desc(version, "gh")) {
          version <- "nightly"
        }

        version <- check_version(version)
        self$compose$parsed <- set_compose_image(self$compose$parsed, version)
        private$.write()
      }

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

      if (isTRUE(self$is_built())) self$down()
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


    ## Compose ----
    #' @description
    #' Set a name for the ORS container.
    #'
    #' @param name Name for the ORS container. If \code{NULL}, generates
    #' a random name (\code{"ors-appXXXX"}).
    set_name = function(name = NULL) {
      assert_that(is_string(name, null = TRUE))

      old <- self$compose$parsed$services$`ors-app`$container_name
      new <- name %||% random_ors_name(private, name)

      if (!identical(old, new)) {
        ors_cli(info = list(c("*" = "Setting name to {.val {new}}")))
        self$compose$parsed$services$`ors-app`$container_name <- new
        self$compose$name <- new
        self$update()
      }

      invisible(self)
    },

    #' @description
    #' Set a port for the localhost of the ORS container.
    #'
    #' @param port \code{[numeric]/\code{NULL}}
    #'
    #' Port to use for the container. Can be a vector of length 1 or 2.
    #' The first port is for the API, the second port is optionally for
    #' additional monitoring. If \code{NULL}, assigns a random port using
    #' \code{\link[httpuv:randomPort]{randomPort()}}.
    set_port = function(port = NULL) {
      assert_that(is_integerish(port, null = TRUE), length(port) <= 2)

      new <- as.character(port %||% random_port(2))
      old <- self$compose$ports[1, seq_along(new)]


      if (!identical(old, new)) {
        ors_cli(info = list(c(
          "*" = "Setting {cli::qty(length(new))} port{?s} to {.val {new}}"
        )))
        compose <- self$compose$parsed
        self$compose$parsed$services$`ors-app`$ports <- format_ports(self, new)
        self$compose$ports[1, seq_along(new)] <- new
        self$update()
      }

      invisible(self)
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
    set_memory = function(init = NULL, max = NULL) {
      assert_that(is_number(init, null = TRUE), is_number(max, null = TRUE))

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
        self$compose$parsed$services$`ors-app`$environment <- format_memory(self, new)
        self$compose$memory$init <- new[1] / 1000
        self$compose$memory$max <- new[2] / 1000
        self$update()
      }

      invisible(self)
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
      assert_that(is_true_or_false(mode))

      old <- self$compose$graph_building
      new <- mode

      if (!identical(old, new)) {
        verb <- ifelse(mode, "Enabling", "Disabling")
        ors_cli(info = list(c("*" = "{verb} graph building")))
        self$compose$parsed$services$`ors-app`$environment[1] <- set_gp(self, mode)
        self$compose$rebuild_graphs <- mode
        self$update()
      }

      invisible(self)
    },

    #' @description
    #' Set version of the ORS Docker image. This should preferably be compatible
    #' with the compose version.
    #'
    #' @param version \code{[character]}
    #'
    #' Version specification of the ORS image.
    set_image = function(version) {
      assert_that(is_string(version))

      old <- self$compose$image
      new <- check_version(version) %||% old

      if (!identical(old, new)) {
        ors_cli(info = list(c("*" = "Setting image version to {.field {new}}")))
        self$compose$parsed$services$`ors-app`$image <- paste0(
          "openrouteservice/openrouteservice:v", new
        )
        self$compose$image <- new
        self$update()
      }

      invisible(self)
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
        logs <- cli::ansi_strwrap(logs, width = cli::console_width(), exdent = 2)
      } else {
        logs <- cli::ansi_strip(logs)
      }

      class(logs) <- "ors_logs"
      logs
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
    },

    #' @description
    #' Checks if ORS is initialized. ORS is initialized if it was built
    #' for the first time. An initialized ORS instance has a subdirectory
    #' called \code{"graphs"} that contains built graphs for at least one
    #' routing profile. \code{$is_init()} therefore checks for the
    #' existence of at least one sub-directory of \code{"graphs"}.
    is_init = function() {
      graphs <- file.path(self$paths$top, "graphs")
      length(list.dirs(graphs, recursive = FALSE)) > 0
    }
  ),

  # Private ----
  private = list(
    .overwrite = FALSE,
    .verbose = TRUE,
    .prompts = TRUE,
    .alive = TRUE,

    .parse = function() {
      self$paths <- private$.construct("paths")
      self$compose <- private$.construct("compose")
      self$extract <- private$.construct("extract")
      self$config <- private$.construct("config")
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
        if (difft > 180L) cli::cli_abort(
          "Docker startup timed out.",
          class = "ors_docker_timeout"
        )
      }
    } else {
      cli::cli_abort(
        "Something went wrong while starting Docker. Is it installed?",
        class = "ors_docker_corrupt_installation_error"
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


check_ors_dir <- function(dir, type) {
  switch(
    type,
    docker = {
      check <- file.exists(file.path(dir, "docker-compose.yml"))
      if (!check) {
        cli::cli_abort(c(
          "!" = "OpenRouteService directory is corrupted",
          "i" = "Compose file is missing."
        ), class = "ors_corrupted_dir_error")
      }
    },

    jar = {
      check <- file.exists(file.path(dir, "ors.jar"))
      if (!check) {
        cli::cli_abort(c(
          "!" = "OpenRouteService directory is corrupted",
          "i" = "JAR file is missing."
        ), class = "ors_corrupted_dir_error")
      }
    }
  )
}
