#' Change the ORS configuration
#' 
#' @description Activate profiles and make other changes to the OpenRouteService
#' configuration file. You can either make changes by passing a parsed
#' configuration file as contained in \code{ors_instance} objects or by
#' using one of the convenience arguments.
#' 
#' @param profiles List of characters specifying the active profiles of an
#' instance. By default, can be one of the following: \code{car}, \code{hgv},
#' \code{bike-regular}, \code{bike-mountain}, \code{bike-road},
#' \code{bike-electric}, \code{walking}, \code{hiking}, \code{wheelchair}.
#' @param ... Key-value pairs containing profile-specific routing options.
#' The argument name is interpreted as the profile whose configuration should be
#' changed. Pass \code{"default"} to change the default parameters. The argument
#' value needs to be a list of options to be changed.
#' Example: \code{car = c(maximum_distance = 100000000)}
#' @param matrix.maximum_routes Maximum number of routes that can be computed
#' at once. Defaults to 100.
#' @param matrix.maximum_search_radius Maximum allowed distance between a
#' coordinate and the nearest road. Defaults to 5000 m.
#' @param isochrones.maximum_intervals Maximum number of intervals for each
#' location. Defaults to 10.
#' @param isochrones.maximum_locations Maximum number of locations per request.
#' Defaults to 2.
#' @param config Parsed configuration file as contained in \code{ors_instance}
#' objects or path to a configuration file.
#' @inheritParams ors_extract
#' 
#' @returns Nested list of class \code{ors_instance}.
#' 
#' @family ORS setup functions
#' 
#' @export
ors_config <- function(
  instance,
  profiles = NULL,
  ...,
  matrix.maximum_routes = NULL,
  matrix.maximum_search_radius = NULL,
  isochrones.maximum_intervals = NULL,
  isochrones.maximum_locations = NULL,
  config = NULL,
  interactive = FALSE
) {
  verbose <- attr(instance, "verbose")
  
  if (!interactive) {
    if (is.null(config)) {
      config <- instance$config$parsed
      config_file <- NULL
    } else {
      if (file.exists(config)) {
        config_file <- config
      }
    }
    
    if (...length()) {
      config <- apply_config_dots(config, ...)
    }
    
    if (!is.null(profiles)) {
      profiles <- as.list(profiles)
      config$ors$services$routing$profiles$active <- profiles
    }
    
    if (!is.null(matrix.maximum_routes)) {
      config$ors$services$matrix$maximum_routes <- matrix.maximum_routes
    }
    
    if (!is.null(matrix.maximum_search_radius)) {
      config$ors$services$matrix$maximum_search_radius <- matrix.maximum_search_radius
    }
    
    if (!is.null(isochrones.maximum_intervals)) {
      config$ors$services$isochrones$maximum_intervals <- isochrones.maximum_intervals
    }
    
    if (!is.null(isochrones.maximum_locations)) {
      config$ors$services$isochrones$maximum_locations <- isochrones.maximum_locations
    }
    
    config$ors$services$routing$sources[[1]] <- "data/osm_file.pbf"
    config$ors$services$routing$profiles$default_params$elevation_cache_path <- "data/elevation_cache"
    config$ors$services$routing$profiles$default_params$graphs_root_path <- "data/graphs"
    config$ors$services$routing$init_threads <- 1L
    
    write_config(config, instance$paths$config_path)
  } else {
    slow_edit(instance$paths$config_path, editor = "internal")
  }

  
  instance[["config"]] <- NULL
  
  instance <- .instance(instance, config_file = config_file, verbose = verbose)
  
  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


detect_config <- function(dir, config_path = NULL) {
  conf_dir <- file.path(dir, "docker/conf")
  data_dir <- file.path(dir, "docker/data")
  
  in_conf <- file.exists(file.path(conf_dir, "ors-config.json"))
  in_data <- file.exists(file.path(data_dir, "ors-config.json"))

  if (!is.null(config_path)) {
    if (in_conf) {
      copied <- file.rename(config_path, file.path(conf_dir, "ors-config.json"))
    } else {
      copied <- file.rename(config_path, file.path(data_dir, "ors-config.json"))
    }
    
    if (!copied) cli::cli_abort("Config sample could not be copied.")
  }

  # If docker/conf exists, ORS uses it when the docker container
  # is started -> Prefer the conf directory
  if (in_conf) {
    path <- file.path(conf_dir, "ors-config.json")
    
    # If docker/conf does not exist, check if a config file is in
    # docker/data.
  } else if (in_data) {
    path <- file.path(data_dir, "ors-config.json")
    
    # If everything fails, copy the sample config from the ORS backend
  } else {
    config_sample <- file.path(
      dir, "openrouteservice", "src", "main", "resources", "ors-config-sample.json"
    )
    
    copied <- file.rename(config_sample, file.path(data_dir, "ors-config.json"))
    if (!copied) cli::cli_abort("Config sample could not be copied.")
    
    path <- file.path(data_dir, "ors-config.json")
  }
  
  path
}


read_config <- function(config_path, ...) {
  jsonlite::read_json(config_path, ...)
}


write_config <- function(config, config_path) {
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE)
  cat(config_json, file = config_path)
}


get_all_profiles <- function(config) {
  profile_node <- names(config$ors$services$routing$profiles)
  is_profile <- grepl("profile", profile_node)
  gsub("profile-", "", profile_node[is_profile])
}


apply_config_dots <- function(config, ...) {
  dots <- list(...)
  names(dots) <- paste0("profile-", names(dots))
  names(dots)[grepl("default", names(dots))] <- "default_params"
  for (profile in names(dots)) {
    opts <- names(dots[[profile]])
    for (opt in opts) {
      val <- dots[[profile]][[opt]]
      if (profile == "default_params") {
        config$ors$services$routing$profiles[[profile]][[opt]] <- opts[opt]
      } else {
        config$ors$services$routing$profiles[[profile]]$parameters[[opt]] <- opts[opt]
      }
    }
  }
  config
}
