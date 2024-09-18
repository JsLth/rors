#' ORS JAR instance
#' @description
#' Creates a new ORS instance object that operates on a JAR instance of
#' OpenRouteservice, i.e. an instance that is started using a .jar file.
#' This R6 class is typically constructed by \code{\link{ors_instance}}.
#' \code{ORSJar} requires JDK >= 17 to be installed on the system. If this
#' requirement cannot be met, consider using \code{\link{ORSDocker}} or
#' \code{\link{ORSWar}}.
#'
#' For technical details on the setup of local ORS instances, refer to the
#' \href{https://giscience.github.io/openrouteservice/run-instance/running-jar}{Running JAR documentation}.
#' For details on how to use \code{ORSJar} objects, refer to the installation
#' vignette:
#'
#' \preformatted{
#' vignette("ors-installation", package = "rors")
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # create a new ORS instance of type JAR
#' # downloads ors.jar to a new directory
#' ors <- ors_instance(type = "jar")
#'
#' # set a random port to ensure that the port is not used
#' ors$set_port()
#'
#' # download and set an OSM file
#' ors$set_extract("Rutland")
#'
#' # run an OpenRouteService server
#' ors$up()
#'
#' # the external process that runs the server is stored in the object
#' ors$proc$is_alive() # should return true
#'
#' # retrieve runtime logs
#' ors$show_logs()
#'
#' # stop the server
#' ors$stop()
#' }
ORSJar <- R6Class(
  classname = "ORSJar",
  inherit = ORSLocal,

  # Public ----
  public = list(
    #' @field paths List of relevant file paths for the ORS setup. Includes
    #' the top directory, config file, and extract file.
    paths = NULL,

    #' @field proc \code{processx} process that is used to control the external
    #' process running the OpenRouteService instance. Refer to
    #' \code{\link[processx]{process}} for a reference of methods.
    proc = NULL,

    ## Initialize ----
    #' @description
    #' Initialize the ORSJar object.
    #'
    #' @param dir \code{[character]}
    #'
    #' Custom OpenRouteService directory. If not specified, the jar file will
    #' be downloaded to the current working directory. If a directory called
    #' \code{"openrouteservice-{version}"} is present, the download will be skipped.
    #' Ignored if \code{server} is not \code{NULL}.
    #' @param version \code{[character]}
    #'
    #' The OpenRouteService version to use. Can either be a version number (e.g.
    #' 8.1.1) or \code{"master"}. Defaults to the most recent supported version.
    #' @param overwrite \code{[logical]}
    #'
    #' Whether to overwrite the current OpenRouteService directory if it exists.
    #' @param dry \code{[logical]}
    #'
    #' Whether to start a instance, i.e. initialize a \code{ORSJar}
    #' instance without downloading the executable. This allows you to
    #' manipulate config and extract files but does not allow you to interact
    #' with java.
    #' @param verbose \code{[logical]}
    #'
    #' Level of verbosity. If \code{TRUE}, shows informative warnings and messages,
    #' spinners, progress bars and system notifications.
    #' @param ... Not used.
    initialize = function(dir,
                          version = NULL,
                          overwrite = FALSE,
                          dry = FALSE,
                          verbose = TRUE,
                          ...) {
      version <- version %||% ORS_VERSION
      check_jdk_version(verbose)
      dir <- get_ors_release(dir, version, file = "jar", overwrite, verbose)

      self$paths$top <- dir
      private$.version <- version
      private$.overwrite <- overwrite
      private$.verbose <- verbose
      private$.parse()

      # ensure that no osm file is set by default
      self$config$parsed$ors$engine$source_file <- NULL
      self$update()

      invisible(self)
    },

    #' @description
    #' Purge ORS instance, i.e. take down container, (optionally) delete
    #' image, delete ORS directory, and clean up R6 class.
    #'
    #' This method can be useful for testing and writing reproducible
    #' examples and can easily be used together with \code{\link{on.exit}}.
    purge = function() {
      if (!private$.alive) {
        return(invisible(NULL))
      }

      if (private$.prompts) {
        ors_cli(info = list(c("i" = paste(
          "Purging the current instance kills the running service process,",
          "ORS directory and cleans up the R6 object."
        ))))

        yes_no("Do you want to proceed?", no = cancel())
      }

      ors_cli(h2 = "Purging ORS")
      self$stop()

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
      self$config <- NULL
      self$paths <- NULL
      private$.alive <- FALSE
      invisible(NULL)
    },

    ## Config ----
    #' @description
    #' Set a HTTP port for the spring server.
    #'
    #' @param port \code{[numeric]}/\code{NULL}
    #'
    #' Port to use. If \code{NULL}, assigns a random port using
    #' \code{\link[httpuv:randomPort]{randomPort()}}.
    set_port = function(port = NULL) {
      assert_that(is_number(port, null = TRUE))

      new <- port %||% random_port()
      old <- self$config$parsed$server$port

      if (!identical(old, new)) {
        ors_cli(info = list(c(
          "*" = "Setting {cli::qty(length(new))} port{?s} to {.val {new}}"
        )))
        self$config$parsed$server$port <- new
        self$update()
      }

      invisible(self)
    },

    #' @description
    #' Show logs of OpenRouteService. Useful for debugging docker setups.
    #'
    #' @param source \code{[logical]}
    #'
    #' Where to retrieve logs from. If \code{"process"}, reads the standard
    #' output from the external process running OpenRouteService. If
    #' \code{"logfile"}, reads the log file in the top directory. Generally,
    #' the log file provides less information, but stores the logs from
    #' previous runs. The process logs are reset with every run. Process logs
    #' are also much more error-prone.
    show_logs = function(source = c("logfile", "process")) {
      source <- match.arg(source)
      logs <- switch(
        source,
        process = split_by_log_entry(self$proc$read_output()),
        logfile = read_logfile(self$paths$top)
      )
      class(logs) <- "ors_logs"
      logs
    },

    ## Control ----
    #' @description
    #' Run ors.jar to start a local OpenRouteService server. Creates an
    #' attribute of \code{ORSJar} called \code{proc} which stores the external
    #' process used to run the server. A running server can (and should) be
    #' stopped using the \code{$stop()} method.
    #'
    #' @param wait \code{[wait]}
    #'
    #' Whether to wait for the server to start and then give out a visual
    #' and audible notification. If \code{FALSE}, returns control of the
    #' console back to the user. The status of the OpenRouteService server
    #' can then be polled using the \code{proc} attribute.
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
    #' @param ... Further options passed to the
    #' \href{https://docs.oracle.com/en/java/javase/17/docs/specs/man/java.html}{\code{java}} command.
    up = function(wait = TRUE, init = NULL, max = NULL, ...) {
      run_ors_jar(self, private, wait = wait, ...)
    },

    #' @description
    #' Stops a running OpenRouteService instance by killing the external
    #' process.
    stop = function() {
      if (self$is_running()) {
        ors_cli(info = list(c("i" = "Killing OpenRouteService process.")))
        self$proc$kill_tree()
      }
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
    },

    #' @description
    #' Checks if the ORS process is running. You can control this state using
    #' \code{$up()} and \code{$stop()}. Check \code{$is_ready()} to see
    #' if the ORS setup succeeded.
    is_running = function() {
      if (is.null(self$proc)) {
        return(FALSE)
      }
      self$proc$is_alive()
    }
  ),

  # Private ----
  private = list(
    .alive = TRUE,
    .dir = NULL,
    .version = NULL,
    .overwrite = NULL,
    .verbose = NULL,

    finalize = function() {
      if (inherits(self$proc, "process")) {
        self$proc$kill_tree()
      }
    }
  )
)


get_java_version <- function(verbose) {
  if (!has_util("java")) {
    url <- "https://www.oracle.com/de/java/technologies/downloads/"
    msg <- c(
      "!" = "JDK required but not found.",
      "i" = "Download JDK from {.url {url}}."
    )
    abort(msg, class = "java_missing_error", call = NULL)
  }

  version <- callr::run("java", "-version", error_on_status = TRUE)$stderr
  version <- gsub("\n", "\f", gsub("\r", "", version))
  cli_once("java_msg", msg = version, verbose = verbose)
  version
}


# regex borrowed from https://github.com/e-kotov/rJavaEnv/blob/main/R/java_env.R
check_jdk_version <- function(verbose) {
  version <- get_java_version(verbose)
  version <- regex_match(
    version,
    "(?<=(openjdk|java) (version \\\")?)[0-9]{1,2}(?=\\.)",
    perl = TRUE
  )[[1]][1]

  if (!minimum_version(version, "17")) {
    url <- "https://www.oracle.com/de/java/technologies/downloads/"
    cli::cli_abort(c(
      "!" = "JDK version {version} detected but version 17 required.",
      "i" = "Download JDK from {.url {url}}"
    ))
  }
}


run_ors_jar <- function(self, private, wait = TRUE, ...) {
  verbose <- private$.verbose

  if (self$is_running()) {
    cli::cli_abort(c(
      "!" = "Another OpenRouteService is still running.",
      "i" = "You may stop it using `$stop()`."
    ))
  }

  proc <- callr::process$new(
    "java",
    args = c("-jar", "ors.jar", ...),
    stdout = "|",
    stderr = "|",
    wd = self$paths$top,
    cleanup_tree = TRUE
  )

  error <- proc$read_error()
  if (nzchar(error)) {
    cli::cli_abort("ors.jar setup error: {error}")
  }

  self$proc <- proc

  if (wait) {
    setup_info(verbose)
    withCallingHandlers(
      notify_when_ready(self, private, interval = 10L, verbose = verbose),
      error = function(e) self$stop()
    )
  }
}
