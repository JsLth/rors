# Title     : ORS configuration control panel
# Objective : Change ORS configurations
# Created by: Jonas Lieth
# Created on: 17.08.2021



#' OpenRouteService configuration control panel
#' @description R6 class that loads the ORS config file and can be used to
#' change the ORS configurations
#'
#' @details The argument `profiles` refers to the supported modes of transport.
#' Avoid passing all profiles as each profile has to be built seperately, which
#' can strain memory extremely quickly. For a list of and details on the
#' supported profiles, refer to the
#' \href{https://giscience.github.io/openrouteservice/documentation/Tag-Filtering.html}{OpenRouteService documentation}.
#' For a details on each configuration in the config file, refer to the
#' \href{https://giscience.github.io/openrouteservice/installation/Configuration.html}{config documentation}.
#'
#' @seealso \code{\link{ORSInstance}}
#'
#' @importFrom magrittr %>%

ORSConfig <- R6::R6Class(
  classname = "ORSConfig",
  inherit = ORSInstance,
  active = list(

    #' @field active_profiles Currently active profiles in the config file.
    #' By assigning a character vector, the field changes the active profiles
    #' in the config file.
    active_profiles = function(profiles) ORSConfig$funs$active_profiles(self, private, profiles)
  ),

  public = list(

    #' @field ors_config JSON config file, parsed as a list. Objects and items
    #' can be changed by assigning values to them.
    ors_config = NULL,

    #' @field path Path to the config file. Usually, this is either docker/conf
    #' if the ORS image was already built or docker/data if the image is yet to
    #' be built for the first time.
    path = NULL,

    #' @description Initializes the `ORSConfig` class. Specifies the config
    #' path, copies the config files if necessary and reads them.
    initialize = function() {
      conf_dir <- file.path(self$dir, "docker/conf")
      data_dir <- file.path(self$dir, "docker/data")

      # If docker/conf exists, ORS uses it when the docker container
      # is started -> Prefer the conf directory
      if (dir.exists(conf_dir)) {
        config <- jsonlite::read_json(file.path(conf_dir,
                                                "ors-config.json"))
        self$path <- file.path(conf_dir, "ors-config.json")

      # If docker/conf does not exist, check if a config file is in
      # docker/data.
      } else if (file.exists(file.path(data_dir, "ors-config.json"))) {
        config <- jsonlite::read_json(file.path(data_dir, "ors-config.json"))
        self$path <- file.path(data_dir, "ors-config.json")

      # If all fails, copy the sample config from the ORS backend
      } else {
        config_sample <- file.path("openrouteservice",
                                   "src",
                                   "main",
                                   "resources",
                                   "ors-config-sample.json")

        file.copy(file.path(self$dir, config_sample), data_dir)
        file.rename(file.path(data_dir, "ors-config-sample.json"),
                    file.path(data_dir, "ors-config.json"))

        config <- jsonlite::read_json(file.path(data_dir, "ors-config.json"))
        self$path <- file.path(data_dir, "ors-config.json")
      }

      self$ors_config <- config

      invisible(self)
    },

    #' @description Saves the config changes by overwriting the config files
    #' with all changed fields. This should be run each time after changing
    #' any configurations.

    save_config = function() ORSConfig$funs$save_config(self),

    #' @description Opens the raw config file to allow manual changes. Useful
    #' if you find the list structure of the parsed JSON impractical.

    open_config = function() ORSConfig$funs$open_config(self)
  ),

  private = list(
    .translate_profiles = function(profile) ORSConfig$funs$translate_profiles(profile)
  ),
  cloneable = FALSE
)


ORSConfig$funs <- new.env()

# Public methods --------------------------------------------------------------

ORSConfig$funs$active_profiles <- function(self, private, profiles) {
  if (missing(profiles)) {
    profiles <- self$ors_config$ors$services$routing$profiles$active
  } else {
    cli_abortifnot(is.character(profiles))

    profiles <- private$.translate_profiles(profiles) %>%
      .[!duplicated(.)]

    if (length(profiles) > 0) {
      self$ors_config$ors$services$routing$profiles$active <- as.list(profiles)
    }
    self$save_config()
  }
  assign("profiles", profiles, envir = pkg_cache)
}


ORSConfig$funs$save_config <- function(self) {
  config_json <- jsonlite::toJSON(self$ors_config,
                                  auto_unbox = TRUE,
                                  pretty = TRUE)

  cat(config_json, file = self$path)
}


ORSConfig$funs$open_config <- function(self) {
  file.open(normalizePath(self$path, winslash = "/"))
}


# Private methods -------------------------------------------------------------

ORSConfig$funs$translate_profiles <- function(profiles) {
  translator <- data.frame(
    normal_names = c(
      "driving-car", "driving-hgv", "cycling-regular", "cycling-mountain",
      "cycling-road", "cycling-electric", "foot-walking", "foot-hiking",
      "wheelchair"
    ),
    config_names = c(
      "car", "hgv", "bike-regular", "bike-mountain", "bike-road",
      "bike-electric", "walking", "hiking", "wheelchair"
    )
  )

  translate <- function(profile) {
    if (is.element(profile, translator$normal_names)) {
      translator$config_names[translator$normal_names == profile]
    } else if (is.element(profile, translator$config_names)) {
      profile
    } else {
      cli::cli_warn(paste("Profile {.val {profile}} does not conform to the",
                          "ORS naming scheme and will be skipped."))
      NULL
    }
  }

  translated_profiles <- lapply(profiles, translate) %>%
    purrr::discard(is.null) %>%
    unname()
}