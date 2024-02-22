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
    #' @param token \code{[logical]}
    #'
    #' Whether \code{server} requires authorization over a token. ORS tokens
    #' are stored in the \code{ORS_TOKEN} environment variable. Defaults to
    #' \code{FALSE}. If \code{server} is the public API, \code{token} is set
    #' to \code{TRUE}.
    initialize = function(server, token = FALSE) {
      assert_that(is.character(server), is_true_or_false(token))

      if (server %in% c("pub", "public", public_api)) {
        server <- public_api
        token <- TRUE
      }

      if (!nzchar(get_ors_token()) && token) {
        link <- "https://openrouteservice.org/"
        link <- cli::style_hyperlink(link, link)
        cli::cli_abort(c(
          "!" = "The public API requires an API token!",
          "i" = paste(
            "Request one under {.url {link}} and store it",
            "in the {.code ORS_TOKEN} environment variable."
          )
        ))
      }

      if (nzchar(get_ors_token())) {
        token <- ors_token(token)
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


ors_token <- function(active = FALSE) {
  structure(nzchar(get_ors_token()), class = "ors_token", active = active)
}


needs_token <- function(x) {
  if (inherits(x, "ors_token")) {
    isTRUE(attr(x, "active"))
  } else {
    FALSE
  }
}


public_api <- "https://api.openrouteservice.org/"


#' @export
print.ors_token <- function(x, ...) {
  active <- attr(x, "active")

  if (active) {
    emph <- cli::col_red("requires")
    msg1 <- cli::format_message(c("i" = paste(
      "This instance", emph, "a token."
    )))

    if (x) {
      msg2 <- cli::format_message(c(
        "v" = "A token is stored in the `ORS_TOKEN` environment variable."
      ))
    } else {
      msg2 <- cli::format_message(c(
        "x" = "No token found in the `ORS_TOKEN` environment variable."
      ))
    }
  } else {
    emph <- cli::style_underline("no")
    msg1 <- cli::format_message(c("i" = paste(
      "This instance requires", emph, "token."
    )))

    msg2 <- NULL
  }




  cat("<ors_token>", "\n", msg1, "\n", msg2, if (!is.null(msg2)) "\n")
}
