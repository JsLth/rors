#' ORS instance
#' @description
#' This R6 class is the foundation for R6 classes \code{\link{ORSLocal}} and
#' \code{\link{ORSRemote}} and cannot be initialized. It defines methods to
#' check status and information of a remote ORS server and to mount itself to
#' the R session.
#'
ORSInstance <- R6::R6Class(
  classname = "ORSInstance",

  # Public ----
  public = list(
    #' @description
    #' Checks if ORS is ready to use. If \code{TRUE}, the \link{ORSRouting}
    #' routing functions can be used.
    is_ready = function() {
      ors_ready(id = private$.get_id())
    },

    #' @description
    #' Retrieves information about the ORS server.
    get_status = function() {
      if (self$is_ready()) {
        get_status(id = private$.get_id())
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
    }
  ),

  # Private ----
  private = list(
    .get_id = function() {
      if (inherits(self, "ORSLocal")) {
        self$compose$name
      } else if (inherits(self, "ORSRemote")) {
        self$url
      }
    },

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
#' functions listed below stores the instance in an internal environment object
#' and enables functions like \code{\link{ors_pairwise}} to
#' automatically detect the appropriate server information needed to make a
#' successful request. Hence, this function should always be run after
#' loading \code{ORSRouting} as a means of fixing an instance to the current
#' session.
#'
#' While initializing an instance using an already running server requires no
#' further action, this function family excels at building a local
#' OpenRouteService server from source. Setting up a local server effectively
#' removes any server-side rate limits and allows you to conveniently use the
#' package functions on much larger datasets. For setting up a local server,
#' it is required to build and start a Docker container.
#' To do this, \code{ors_instance}
#' starts Docker (if necessary), downloads and unpacks the OpenRouteService
#' source code and returns an object of class \code{ors_instance}.
#'
#' For a full reference of the methods of an ORS instance, refer to
#' \code{\link{ORSLocal}} and \code{\link{ORSRemote}}.
#'
#' @param dir \code{[character]}
#'
#' Custom OpenRouteService directory. If not specified, the directory
#' will be downloaded to the system's home directory. If a directory called
#' \code{"openrouteservice-{version}"} is present, the download will be skipped.
#' Ignored if \code{server} is not \code{NULL}.
#' @param server \code{[character]}
#'
#' URL of a server that accepts OpenRouteService requests. This can be a URL
#' to a local or a remote server. The official public API can be accessed using
#' the shortcut \code{"public"}. Keep in mind that the public API is
#' rate-restricted and requests are automatically throttled to 40 requests per
#' minute. Routing functions \emph{will} be slow for larger datasets.
#' @param version \code{[character]}
#'
#' The OpenRouteService version to use. Can either be a version number (e.g.
#' 7.2.0), a commit hash, or \code{"master"}. Ignored if \code{server} is not
#' \code{NULL}.
#' @param overwrite \code{[logical]}
#'
#' Whether to overwrite the current OpenRouteService directory
#' if it exists.
#' @param verbose \code{[logical]}
#'
#' If \code{TRUE}, prints informative messages and spinners.
#'
#' @returns R6 object of class \code{ors_instance} as created by
#' \code{\link{ORSInstance}}
#'
#' @examples
#' \dontrun{
#' dir <- dir.create("~/test_ors")
#'
#' # Download and furnish an ORS instance
#' ors_instance(dir = dir, version = "7.2.0")
#'
#' # Connect to the public API
#' ors_instance(server = "public")
#'
#' # Connect to a local server
#' ors_instance(server = "https://127.0.0.1:8001/")
#' }
#'
#' @export
ors_instance <- function(dir = "~",
                         server = NULL,
                         version = "latest",
                         overwrite = FALSE,
                         verbose = TRUE) {
  if (!is.null(server)) {
    ORSRemote$new(server = server)
  } else {
    ORSLocal$new(
      dir = dir,
      version = version,
      overwrite = overwrite,
      verbose = verbose
    )
  }
}
