#' ORS instance
#' @description
#' This R6 class is the foundation for R6 classes \code{\link{ORSLocal}} and
#' \code{\link{ORSRemote}} and cannot be initialized. It defines methods to
#' check status and information of a remote ORS server and to mount itself to
#' the R session.
ORSInstance <- R6Class(
  classname = "ORSInstance",

  # Public ----
  public = list(
    #' @description
    #' Checks if ORS is ready to use. If \code{TRUE}, the
    #' routing functions can be used.
    is_ready = function() {
      ors_ready(self$get_url())
    },

    #' @description
    #' Retrieves information about the ORS server.
    get_status = function() {
      if (self$is_ready()) {
        ors_status(self$get_url())
      }
    },

    #' @description
    #' Checks whether the instance is currently mounted to the session.
    #' More technically, \code{$is_mounted} compares the MD5 hashes of its
    #' R6 object and the currently mounted instance. Hence, an instance
    #' only counts as mounted if it is exactly the same as the currently
    #' mounted instance. An instance that was manually manipulated and has
    #' not been updated with \code{$update()} thus is not considered
    #' mounted.
    is_mounted = function() {
      identical(private$.get_hash(), cli::hash_obj_md5(get_instance()))
    },

    #' @description
    #' Constructs the URL that is used to communicate with OpenRouteService.
    #' Except for \code{ORSRemote}, the output is an aspirational URL, i.e. it
    #' only works after a successful setup.
    get_url = function() {
      cls <- class(self)
      if (any(c("ORSJar", "ORSWar") %in% cls)) {
        port <- self$config$parsed$server$port %||% 8082
        host <- "localhost"
        sprintf("http://%s:%s/", host, port)
      } else if ("ORSDocker" %in% cls) {
        sprintf("http://localhost:%s/", self$compose$ports[1, 1])
      } else if ("ORSRemote" %in% cls) {
        self$url
      }
    }
  ),

  # Private ----
  private = list(
    .get_hash = function() {
      cli::hash_obj_md5(self)
    },

    .mount = function() {
      assign("instance", self, envir = ors_cache)
    }
  )
)


# Wrapper ----
#' OpenRouteService instance
#'
#' @description Creates an OpenRouteService instance object. An instance
#' represents either a local/remote server or a directory from which
#' OpenRouteService can be set up. Running this function or any of the related
#' functions listed below stores the instance in an internal environment
#' and enables functions interacting with the OpenRouteService API to
#' automatically detect the appropriate server information needed to make a
#' successful request. Hence, this function should always be run after
#' loading \code{rors} as a means of fixing an instance to the current
#' session.
#'
#' While initializing an instance using an already running server requires no
#' further action, this function family excels at building a local
#' OpenRouteService server from source. Setting up a local server effectively
#' removes any server-side rate limits and allows you to conveniently use the
#' package functions on much larger datasets. For setting up a local server,
#' it is required to build and start a Docker container.
#' To do this, \code{ors_instance}
#' starts Docker (if necessary), creates an ORS directory, parses all
#' important information and jumpstarts a fresh ORS server.
#'
#' For a full reference of the methods of an ORS instance, refer to
#' \code{\link{ORSLocal}} and \code{\link{ORSRemote}}.
#'
#' @param dir \code{[character]}
#'
#' Custom OpenRouteService directory. If not specified, the directory
#' will be downloaded to the working direcotry. If a directory called
#' \code{"openrouteservice-{version}"} is present, the download will be skipped
#' unless overwrite is \code{TRUE}.
#' Ignored if \code{server} is given.
#' @param server \code{[character]}
#'
#' URL of a server that accepts OpenRouteService requests. This can be a URL
#' to a local or a remote server. The official public API can be accessed using
#' the shortcuts \code{"public"} or \code{"pub"}. Keep in mind that the public
#' API is rate-restricted and requests are automatically throttled to 40
#' requests per minute. Routing functions \emph{will} be slow for larger
#' datasets.
#'
#' @param type \code{[character]}
#'
#' The type of application to set up. Can be one of \code{"docker"} (default),
#' \code{"jar"}, or \code{"war"}. Docker instances are the most versatile but
#' also the most complex. Docker instances require Docker to be installed and
#' accessible (relevant for Linux users). JAR instances require JDK >= 17 to
#' be installed. WAR instances require Tomcat 10 to be installed. JAR
#' instances are controlled using an external process and are thus
#' bound to the R session. If the session dies, so do the OpenRouteService
#' servers. Docker containers, conversely, are not bound to the R session.
#' When using Docker Desktop (on Windows) and starting the Docker daemon from
#' R, the software closes when R dies. This stops ORS containers but does not
#' kill them.
#'
#' @param ... Further arguments passed to \code{\link{ORSLocal}} or
#' \code{\link{ORSRemote}}.
#'
#' @returns R6 object of class \code{ors_instance} as created by
#' \code{\link{ORSLocal}} or \code{\link{ORSRemote}}.
#'
#' @section Session mounting:
#'
#' Upon initialization (or whenever calling a method) the object created by
#' \code{ors_instance} mounts itself to the current R session.
#' \code{ors_*} functions automatically identify the mounted ORS instance to
#' retrieve important information for querying ORS servers or processing other
#' data. Mounted instances are cleared when starting a new session or
#' when overwriting with a new instance.
#'
#' In case multiple instances are running simultaneously and should be queried
#' in the same instance, \code{ors_*} functions allow to directly pass an
#' \code{instance} object. Otherwise, it is recommended to implicitly use
#' the mounted instance.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Download and furnish an ORS instance
#' ors_instance(dir = tempdir(), version = "8.0.0")
#'
#' # Connect to the public API
#' Sys.setenv(ORS_TOKEN = "xxx") # a token must be set
#' ors_instance(server = "public")
#'
#' # Connect to a local server
#' # Initializing an instance by passing the URL to a local server
#' # does not allow for control of the underlying data and
#' # configurations. If finer control over such things is needed,
#' # it is necessary to set up the instance from scratch using the
#' # `dir` argument.
#' ors_instance(server = "https://127.0.0.1:8001/")
#' }
ors_instance <- function(dir = ".",
                         server = NULL,
                         type = c("docker", "jar", "war"),
                         ...) {
  type <- match.arg(type)
  if (!is.null(server)) {
    ORSRemote$new(server = server, ...)
  } else {
    switch(
      type,
      jar = ORSJar$new(dir = dir, ...),
      war = ORSWar$new(dir = dir, ...),
      docker = ORSDocker$new(dir = dir, ...)
    )
  }
}
