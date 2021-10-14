# Title     : Data formatting
# Objective : Get datasets into shape for processing in the main functions
# Created by: Jonas Lieth
# Created on: 14.10.2021


format_input_data <- function(data) {
  if (is.sf(data)) {
    data <- reformat_vectordata(data)[, c("X", "Y")]
  } else {
    if (is.matrix(data) || is.list(data)) {
      data <- as.data.frame(data)
    }
    if (ncol(data) > 2) {
      if (all(is.element(c("X", "Y"), colnames(data)))) {
        data <- data[, c("X", "Y")]
      } else if (all(is.element(c("Lon", "Lat"), colnames(data)))) {
        data <- data[, c("Lon", "Lat")]
      } else if (is.numeric(unlist(data[, c(1, 2)]))) {
        data <- data[, c(1, 2)]
      } else {
        cli::cli_abort(paste("Cannot determine coordinate columns of",
                             "dataframe {.var {deparse(substitute(data))}}"))
      }
    }
  }
  data
}



format_ors_options <- function(options, profile) {
  options_check <- NULL

  if (!is.null(options$geometry_simplify)) {
    if (isTRUE(options$geometry_simplify) ||
        isFALSE(options$geometry_simplify)) {
      options_check["geometry_simplify"] <- TRUE
    }else {
      options_check["geometry_simplify"] <- FALSE
      options$geometry_simplify <- NULL
    }
  }

  if (!is.null(options$continue_straight)) {
    if (isTRUE(options$continue_straight) ||
        isFALSE(options$continue_straight)) {
      options_check["continue_straight"] <- TRUE
    } else {
      options_check["continue_straight"] <- FALSE
      options$continue_straight <- NULL
    }
  }

  if (!is.null(options$avoid_borders)) {
    if (is.character(options$avoid_borders) &&
        length(options$avoid_borders) == 1 &&
        is.element(options$avoid_borders, c("all", "controlled", "none")) &&
        identical(base_profile(profile), "driving")) {
      options_check["avoid_borders"] <- TRUE
    } else {
      options_check["avoid_borders"] <- FALSE
      options$avoid_borders <- NULL
    }
  }

  if (!is.null(options$avoid_countries)) {
    if (is.numeric(options$avoid_countries) &&
        identical(base_profile(profile), "driving")) {
      options$avoid_countries <- list(options$avoid_countries)
      options_check["avoid_countries"] <- TRUE
    } else {
      options_check["avoid_countries"] <- FALSE
      options$avoid_countries <- NULL
    }
  }

  if (!is.null(options$avoid_features)) {
    if (is.character(options$avoid_features)) {
      options$avoid_features <- list(options$avoid_features)
      options_check["avoid_features"] <- TRUE
    } else {
      options_check["avoid_features"] <- FALSE
      options$avoid_features <- NULL
    }
  }

  if (!is.null(options$avoid_polygons)) {
    if (is.sf(options$avoid_polygons) &&
        sf::st_is(options$avoid_polygons, c("POLYGON", "MULTIPOLYGON"))) {
      geom_type <- as.character(sf::st_geometry_type(options$avoid_polygons))
      options$avoid_polygons <- list(
        type = capitalizeChar(geom_type),
        coordinates = list(sf::st_coordinates(options$avoid_polygons))
      )
      options_check["avoid_polygons"] <- TRUE
    } else {
      options_check["avoid_polygons"] <- FALSE
      options$avoid_polygons <- NULL
    }
  }

  if (!is.null(options$profile_params)) {
    if (is.list(options$profile_params) &&
        !identical(profile, "driving-car")) {
      base_profile <- base_profile(profile)
      allowed_opts <- switch(base_profile,
                             wheelchair = c("maximum_incline",
                                            "maximum_sloped_kerb",
                                            "minimum_width",
                                            "smoothness_type",
                                            "surface_type",
                                            "track_type"),
                             hgv        = c("axleload",
                                            "hazmat",
                                            "height",
                                            "length",
                                            "weight",
                                            "width"),
                             cycling    = "steepness_difficulty",
                             walking    = c("green", "quiet"))
      opt_admitted <- purrr::map(options$profile_params,
                                 ~is.element(names(.), allowed_opts))
      options_check["profile_param"] <- all(unlist(opt_admitted))

      if (any(opt_admitted$weightings)) {
        options$
          profile_params$
          weightings <- options$profile_params$weightings[opt_admitted$weightings]
      } else options$profile_params$weightings <- NULL

      if (any(opt_admitted$restrictions)) {
        options$
          profile_params$
          restrictions <- options$profile_params$restrictions[opt_admitted$restrictions]
      } else options$profile_params$restrictions <- NULL
    } else {
      options_check["profile_param"] <- FALSE
      options$profile_params <- NULL
    }
  }

  if (!is.null(options$vehicle_type)) {
    if (is.character(options$vehicle_type) &&
        identical(profile, "driving-hgv")) {
      options$vehicle_type <- list(options$vehicle_type)
      options_check["vehicle_type"] <- TRUE
    } else {
      options_check["vehicle_type"] <- FALSE
      options$vehicle_type <- NULL
    }
  }

  if (!is.null(options$preference)) {
    if (is.character(options$preference) &&
        length(options$preference) == 1 &&
        is.element(options$preference, c("fastest", "shortest", "recommended"))) {
      options_check["preference"] <- TRUE
    } else {
      options_check["preference"] <- FALSE
      options$preference <- NULL
    }
  }

  if (!is.null(options$radiuses)) {
    if (is.numeric(options$radiuses) && length(options$radiuses) == 1) {
      options_check["radiuses"] <- TRUE
    } else {
      options_check["radiuses"] <- FALSE
      options$radiuses <- NULL
    }
  }

  if (!is.null(options$maximum_speed)) {
    if (is.numeric(options$maximum_speed) && length(maximum_speed) == 1) {
      options_check["maximum_speed"] <- TRUE
    } else {
      options_check["maximum_speed"] <- FALSE
      options$maximum_speed <- NULL
    }
  }

  adv_options_list <- list(
    avoid_borders   = options$avoid_borders,
    avoid_countries = options$avoid_countries,
    avoid_features  = options$avoid_features,
    avoid_polygons  = options$avoid_polygons,
    profile_param   = options$profile_params,
    vehicle_type    = options$vehicle_type
  )

  adv_options_list <- adv_options_list[lengths(adv_options_list) > 0]

  options_list <- list(
    geometry_simplify = options$geometry_simplify,
    continue_straight = options$continue_straight,
    options           = adv_options_list,
    preference        = options$preference,
    radiuses          = options$radiuses,
    maximum_speed     = options$maximum_speed
  )

  if (!all(options_check)) {
    cli::cli_warn(paste("The following options are formatted incorrectly and",
                          "will be skipped:"))
    cli::cli_ul(items = names(options_check)[!options_check])
  }

  options_list <- options_list[lengths(options_list) > 0]
  options_list
}
