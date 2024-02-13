#' ORS remote instance
#'
#' @description
#' Creates a new ORS instance object. This R6 class is typically
#' constructed by \code{\link{ors_instance}}.
#'
#' This object represents a remote instance, i.e. an existing server like
#' the public API.
#'
#' @export
ORSRemote <- R6::R6Class(
  classname = "ORSRemote",
  inherit = ORSInstance,

  # Public ----
  public = list(
    #' @field url URL to the ORS server
    url = NULL,

    #' @field token Information about API tokens
    token = NULL,

    #' @description
    #' Initialize the remote ORS instance.
    #'
    #' @param server \code{[character]}
    #'
    #' URL of a server that accepts OpenRouteService requests. This can be a URL
    #' to a local or a remote server. The official public API can be accessed using
    #' the shortcut \code{"pub"}. Keep in mind that the public API is
    #' rate-restricted and requests are automatically throttled to 40 requests per
    #' minute. Routing functions \emph{will} be slow for larger datasets.
    #'
    #' @param token Some remote APIs require tokens to function. Although this
    #' is an argument, it is advisable to store API tokens in the environment
    #' variable \code{ORS_TOKEN} which is read internally.
    initialize = function(server, token = NULL) {
      assert_that(is.character(server))

      if (!is.null(token)) {
        cli::cli_warn(paste(
          "Consider storing your ORS token in the {.code ORS_TOKEN}",
          "environment variable in order to keep your API access safe!"
        ))

        Sys.setenv(ORS_TOKEN = token)
      }

      if (server %in% c("pub", public_api)) {
        server <- "https://api.openrouteservice.org/"

        if (!nzchar(get_ors_token())) {
          link <- cli::style_hyperlink(
            "https://openrouteservice.org/", "https://openrouteservice.org/"
          )
          cli::cli_abort(c(
            "!" = "The public API requires an API token!",
            "i" = paste(
              "Request one under {.url {link}} and store it",
              "in the {.code ORS_TOKEN} environment variable."
            )
          ))
        }
      }

      if (nzchar(get_ors_token())) {
        token <- ors_token()
      }

      if (!is_url(server)) {
        cli::cli_abort(
          "{.path {server}} is not a valid URL to an OpenRouteService server"
        )
      }

      self$url <- server
      self$token <- token
      private$.mount()
      invisible(self)
    }
  )
)


get_ors_token <- function() {
  Sys.getenv("ORS_TOKEN")
}


ors_token <- function() {
  structure(nzchar(get_ors_token()), class = "ors_token")
}


public_api <- "https://api.openrouteservice.org/"


#' @export
print.ors_token <- function(x, ...) {
  cat("<ors_token>\n")
  cat("  A token is stored in the `ORS_TOKEN` env variable.")
}
