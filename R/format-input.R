# Title     : Input data formatting
# Objective : Get datasets into shape for processing in the main functions
# Created by: Jonas Lieth
# Created on: 14.10.2021


format_input_data <- function(data) {
  if (is.sf(data)) {
    if (all(sf::st_is(data, c("POINT", "MULTIPOINT")))) {
      data <- sf::st_transform(data, 4326L)
      data <- reformat_vectordata(data)[, c("X", "Y")]
    } else {
      geom_type <- sf::st_geometry_type(data)
      cli::cli_abort("Input data must contain points, not {.val geom_type}.")
    }
  } else {
    if (is.matrix(data) || is.list(data) || is.array(data)) {
      data <- as.data.frame(data)
    } else if (is.double(data) && length(data) == 2L) {
      data <- as.data.frame(t(data))
    } else if (is.null(data) || is.na(data)) {
      data <- NULL
    } else {
      cli::cli_abort(paste("Input data of class {.cls class(data)} is not (yet)",
                           "supported."))
    }
    if (ncol(data) > 2L && !is.null(data)) {
      if (all(is.element(c("X", "Y"), colnames(data)))) {
        data <- data[, c("X", "Y")]
      } else if (all(is.element(c("Lon", "Lat"), colnames(data)))) {
        data <- data[, c("Lon", "Lat")]
      } else if (is.double(unlist(data[, c(1L, 2L)]))) {
        data <- data[, c(1L, 2L)]
      } else {
        cli::cli_abort(paste("Cannot determine coordinate columns of",
                             "dataframe {.var {deparse(substitute(data))}}"))
      }
    }
  }
  data
}



format_ors_options <- function(options, profile) {
  if (is.null(options)) {
    return(NULL)
  }

  options_check <- NULL

  if (!is.null(options$attributes) && length(options$attributes)) {
    attributes <- c("avgspeed", "detourfactor")

    if (isTRUE(options$attributes)) {
      options$attributes <- attributes
    }

    attributes_i <- match(options$attributes, attributes)
    attributes_i <- attributes_i[!is.na(attributes_i)]

    if (length(attributes_i)) {
      options$attributes <- attributes[attributes_i]
      options_check["attributes"] <- TRUE
    }
  }

  if (!is.null(options$elevation)) {
    if (isTRUEorFALSE(options$elevation)) {
      options$elevation <- TRUE
      options_check["elevation"] <- TRUE
    }
  }

  if (!is.null(options$extra_info) && length(options$extra_info)) {
    extra_info <- c("steepness", "suitability", "surface", "waycategory",
                    "waytype", "tollways", "traildifficulty", "osmid",
                    "roadaccessrestrictions", "countryinfo", "green", "noise")

    if (isTRUE(options$extra_info)) {
      options$extra_info <- extra_info
    }

    extra_info_i <- match(options$extra_info, extra_info)
    extra_info_i <- extra_info_i[!is.na(extra_info_i)]

    if (length(extra_info_i)) {
      options$extra_info <- extra_info[extra_info_i]
      options_check["extra_info"] <- TRUE
    } else {
      options_check["extra_info"] <- FALSE
      options$extra_info <- NULL
    }
  }

  if (!is.null(options$continue_straight)) {
    if (isTRUEorFALSE(options$continue_straight)) {
      options_check["continue_straight"] <- TRUE
    } else {
      options_check["continue_straight"] <- FALSE
      options$continue_straight <- NULL
    }
  }

  if (!is.null(options$geometry_simplify)) {
    if (isTRUE(options$geometry_simplify) ||
        isFALSE(options$geometry_simplify)) {
      options_check["geometry_simplify"] <- TRUE
    } else {
      options_check["geometry_simplify"] <- FALSE
      options$geometry_simplify <- NULL
    }
  }

  if (!is.null(options$avoid_borders)) {
    if (is.character(options$avoid_borders) &&
        length(options$avoid_borders) == 1L &&
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
      allowed_opts <- switch(
        base_profile,
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
      opt_admitted <- lapply(
        options$profile_params,
        function(x) is.element(names(x), allowed_opts)
      )
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
        length(options$preference) == 1L &&
        is.element(options$preference, c("fastest", "shortest", "recommended"))) {
      options_check["preference"] <- TRUE
    } else {
      options_check["preference"] <- FALSE
      options$preference <- NULL
    }
  }

  if (!is.null(options$radiuses)) {
    if (is.numeric(options$radiuses) && length(options$radiuses) == 1L) {
      options_check["radiuses"] <- TRUE
    } else {
      options_check["radiuses"] <- FALSE
      options$radiuses <- NULL
    }
  }

  if (!is.null(options$maximum_speed)) {
    if (is.numeric(options$maximum_speed) && length(options$maximum_speed) == 1L) {
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
    profile_params   = options$profile_params,
    vehicle_type    = options$vehicle_type
  )

  adv_options_list <- adv_options_list[lengths(adv_options_list) > 0L]

  options_list <- list(
    attributes        = options$attributes,
    continue_straight = options$continue_straight,
    elevation         = options$elevation,
    extra_info        = options$extra_info,
    geometry_simplify = options$geometry_simplify,
    options           = adv_options_list,
    preference        = options$preference,
    radiuses          = options$radiuses,
    maximum_speed     = options$maximum_speed
  )

  options_list <- options_list[lengths(options_list) > 0L]

  if (!all(options_check)) {
    option_names <- names(options_check)[!options_check]
    cli::cli_warn(paste("The following {length(option_names)} option{?s} ",
                        "{?is/are} formatted incorrectly and will be",
                        "skipped:"))
    cli::cli_ul(items = option_names)
  }

  options_list
}