#' ORS remote instance
#'
#' @description
#' Creates a new ORS instance object. This R6 class is typically
#' constructed by \code{\link{ors_instance}}.
#'
#' This object represents a remote instance, i.e. an existing server like
#' the public API.
#'
#' @param server \code{[character]}
#'
#' URL of a server that accepts OpenRouteService requests. This can be a URL
#' to a local or a remote server. The official public API can be accessed using
#' the shortcuts \code{"public"} or \code{"pub"}. Keep in mind that the public
#' API is rate-restricted and requests are automatically throttled to 40
#' requests per minute. Routing functions \emph{will} be slow for larger
#' datasets.
#'
#' @param token \code{[logical]}
#'
#' Whether \code{server} requires authorization over a token. ORS tokens
#' are stored in the \code{ORS_TOKEN} environment variable. Defaults to
#' \code{FALSE}. If \code{server} is the public API, \code{token} is set
#' to \code{TRUE}.
#'
#' @export
ORSRemote <- R6Class(
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
    initialize = function(server, token = FALSE) {
      assert_that(is.character(server), is_true_or_false(token))

      if (server %in% c("pub", "public", public_api)) {
        server <- public_api
        token <- TRUE
      }

      if (!nzchar(get_ors_token()) && token) {
        link <- "https://openrouteservice.org/"
        link <- cli::style_hyperlink(link, link)
        cli::cli_abort(
          c(
            "!" = "The public API requires an API token!",
            "i" = paste(
              "Request one under {.url {link}} and store it",
              "in the {.code ORS_TOKEN} environment variable."
            )
          ),
          class = "ors_token_missing_error"
        )
      }

      if (nzchar(get_ors_token())) {
        token <- ors_token(token)
      }

      if (!is_url(server)) {
        cli::cli_abort(
          "{.path {server}} is not a valid URL to an OpenRouteService server",
          class = "ors_invalid_server_error"
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


ors_token <- function(active = FALSE) {
  has_token <- nzchar(get_ors_token())
  attr(has_token, "active") <- active
  class(has_token) <- "ors_token"
  has_token
}


needs_token <- function(x) {
  inherits(x, "ors_token") && isTRUE(attr(x, "active"))
}


public_api <- "https://api.openrouteservice.org/"
