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
    active_profiles = function(profiles) {
      if (missing(profiles)) {
        self$ors_config$ors$services$routing$profiles$active
      } else {
        if (is.character(profiles)) {
          profiles <- private$.translate_profiles(profiles) %>%
            .[!duplicated(.)]
          self$
            ors_config$
            ors$
            services$
            routing$
            profiles$
            active <- as.list(profiles)
        } else {
          cli::cli_abort("Pass a valid vector of profiles.")
        }
        self$save_config()
      }
    }
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
      if (basename(getwd()) == "openrouteservice-master") {
        if (dir.exists("docker/conf")) {
          config <- jsonlite::read_json("docker/conf/ors-config.json")
          self$path <- "docker/conf/ors-config.json"
        } else if (file.exists("docker/data/ors-config.json")) {
          config <- jsonlite::read_json("docker/data/ors-config.json")
          self$path <- "docker/data/ors-config.json"
        } else {
          file.copy(
            "openrouteservice/src/main/resources/ors-config-sample.json",
            "docker/data"
          )
          file.rename(
            "docker/data/ors-config-sample.json",
            "docker/data/ors-config.json"
          )
          config <- jsonlite::read_json("docker/data/ors-config.json")
          self$path <- "docker/data/ors-config.json"
        }
        self$ors_config <- config
        self$save_config()
      } else {
        cli::cli_abort(
          "{.cls ORSConfig} must be initialized from the ORS main directory."
        )
      }
      return(self)
    },

    #' @description Saves the config changes by overwriting the config files
    #' with all changed fields. This should be run each time after changing
    #' any configurations.
    save_config = function() {
      config_json <- jsonlite::toJSON(
        self$ors_config,
        auto_unbox = TRUE,
        pretty = TRUE
      )
      if (dir.exists("docker/conf")) {
        cat(config_json, file = "docker/conf/ors-config.json")
      } else {
        cat(config_json, file = "docker/data/ors-config.json")
      }
    },

    #' @description Opens the raw config file to allow manual changes. Useful
    #' if you find the list structure of the parsed JSON impractical.
    open_config = function() {
      shell(normalizePath(self$path, winslash = "\\"))
    }
  ),
  private = list(
    .translate_profiles = function(profiles) {
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
        if (profile %in% translator$normal_names) {
          translator$config_names[translator$normal_names == profile]
        } else if (profile %in% translator$config_names) {
          profile
        } else {
          cli::cli_warn(
            paste(
              "Profile {.val {profile}} does not conform to the ORS",
              "naming scheme and will be skipped."
            )
          )
          NULL
        }
      }
      translated_profiles <- lapply(profiles, translate) %>%
        purrr::discard(is.null) %>%
        unname()
    }
  ),
  cloneable = FALSE
)
