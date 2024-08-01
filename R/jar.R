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
#' For details on how to use \code{ORSDocker} objects, refer to the installation
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
#' ors$run()
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
ORSJar <- R6::R6Class(
  classname = "ORSJar",
  inherit = ORSLocal,

  public = list(
    paths = NULL,

    proc = NULL,

    initialize = function(dir,
                          version = "8.1.1",
                          overwrite = FALSE,
                          verbose = TRUE,
                          ...) {
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
    run = function(wait = TRUE, ...) {
      verbose <- private$.verbose
      proc <- callr::process$new(
        "java",
        args = c("-jar", "ors.jar"),
        stdout = "|",
        stderr = "|",
        wd = self$paths$top,
      )

      error <- proc$read_error()
      if (nzchar(error)) {
        cli::cli_abort("ors.jar setup error: {error}")
      }

      self$proc <- proc

      if (wait) {
        ors_cli(cat = "line", h2 = "Setting up service")
        setup_info(verbose)
        notify_when_ready(
          private$.get_url(),
          type = "jar",
          interval = 10L,
          verbose = verbose,
          warn = TRUE,
          log_dir = self$paths$top
        )
      }
    },

    #' @description
    #' Stops a running OpenRouteService instance by killing the external
    #' process.
    stop = function() {
      self$proc$kill()
    },

    #' @description
    #' Set a HTTP port for the spring server.
    #'
    #' @param port Port to use. If \code{NULL}, assigns a
    #' random port using
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
    #' previous runs. The process logs are reset with every run.
    show_logs = function(source = c("process", "logfile")) {
      read_logs(self$paths$top)
    }
  ),

  private = list(
    .dir = NULL,
    .version = NULL,
    .overwrite = NULL,
    .verbose = NULL,

    .get_url = function() {
      port <- self$config$server$port %||% 8082
      host <- "localhost"
      sprintf("https://%s:%s/", host, port)
    }
  )
)


get_java_version <- function(verbose) {
  version <- callr::run("java", "-version", error_on_status = FALSE)

  if (!identical(version$status, 0L)) {
    url <- "https://www.oracle.com/de/java/technologies/downloads/"
    cli::cli_abort(c(
      "!" = "JDK required but not found.",
      "i" = "Download JDK from {.url {url}}."
    ))
  }

  ors_cli(cat = version$stderr)
  version$stderr
}


check_jdk_version <- function(verbose) {
  version <- get_java_version(verbose)
  version <- strsplit(version, "\n")[[1]]
  version <- version[startsWith(version, "java version")]
  version <- regex_match(version, dQuote("([0-9\\.]+).*", q = FALSE))[[1]][2]

  if (!minimum_version(version, "17")) {
    url <- "https://www.oracle.com/de/java/technologies/downloads/"
    cli::cli_abort(c(
      "!" = "JDK version {version} detected but version 17 required.",
      "i" = "Download JDK from {.url {url}}"
    ))
  }
}
