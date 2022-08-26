format_input_data <- function(.data, to_coords = FALSE) {
  assert_class(.data, c("sf", "sfc"), .env = list(x = substitute(.data)))

  if (all(sf::st_is(.data, c("POINT", "MULTIPOINT")))) {
    .data <- sf::st_transform(.data, 4326L)
    if (to_coords) {
      st_coordinates2(.data)[, c("X", "Y")]
    } else {
      sf::st_as_sf(tibble::as_tibble(.data))
    }
  } else {
    geom_type <- unique(sf::st_geometry_type(.data))
    cli::cli_abort("Input data must contain points, not {.val geom_type}.")
  }
}



format_ors_options <- function(opts, profile) {
  if (is.null(opts)) return(NULL)

  if (!is.null(opts$attributes)) {
    matches <- c("avgspeed", "detourfactor")
    opts$attributes <- format_ors_list(opts$attributes, matches = matches)
  }

  if (!is.null(opts$elevation)) {
    opts$elevation <- format_ors_vector(opts$elevation)
  }

  if (!is.null(opts$extra_info)) {
    matches <- c(
      "steepness", "suitability", "surface", "waycategory",
      "waytype", "tollways", "traildifficulty", "osmid",
      "roadaccessrestrictions", "countryinfo", "green", "noise"
    )
    opts$extra_info <- format_ors_list(opts$extra_info, matches = matches)
  }

  if (!is.null(opts$continue_straight)) {
    opts$continue_straight <- format_ors_vector(opts$continue_straight)
  }

  if (!is.null(opts$geometry_simplify)) {
    opts$geometry_simplify <- format_ors_vector(opts$geometry_simplify)
  }

  if (!is.null(opts$avoid_borders)) {
    format_ors_list(
      opts$avoid_borders,
      matches = c("all", "controlled", "none"),
      profile = c(driving = base_profile(profile)),
      single = TRUE
    )
  }

  if (!is.null(opts$avoid_countries)) {
    opts$avoid_countries <- format_ors_vector(
      opts$avoid_countries,
      type = "numeric",
      profile = c("driving-car" = profile),
      box = TRUE
    )
  }

  if (!is.null(opts$avoid_features)) {
    opts$avoid_features <- format_ors_vector(
      opts$avoid_features,
      type = "character",
      box = TRUE
    )
  }

  if (!is.null(opts$avoid_polygons)) {
    opts$avoid_polygons <- format_ors_poly(opts$avoid_polygons)
  }

  if (!is.null(opts$vehicle_type)) {
    opts$vehicle_type <- format_ors_vector(
      opts$vehicle_type,
      type = "character",
      profile = c("driving-hgv" = profile),
      box = TRUE
    )
  }

  if (!is.null(opts$preference)) {
    opts$preference <- format_ors_list(
      opts$preference,
      matches = c("fastest", "shortest", "recommended"),
      single = TRUE
    )
  }

  if (!is.null(opts$radiuses)) {
    opts$radiuses <- format_ors_vector(opts$radiuses, type = "numeric")
  }

  if (!is.null(opts$maximum_speed)) {
    format_ors_vector(
      opts$maximum_speed,
      type = "numeric",
      single = TRUE
    )
  }

  if (!is.null(opts$allow_unsuitable)) {
    format_ors_vector(
      opts$allow_unsuitable,
      profile = c("wheelchair" = profile)
    )
  }

  if (!is.null(opts$surface_quality_known)) {
    format_ors_vector(
      opts$surface_quality_known,
      profile = c("wheelchair" = profile)
    )
  }

  if (!is.null(opts$restrictions)) {
    opts$restrictions <- format_ors_profile_params(
      opts$restrictions,
      which = "restrictions",
      profile = profile
    )
  }

  if (!is.null(opts$weightings)) {
    opts$weightings <- format_ors_profile_params(
      opts$weightings,
      which = "weightings",
      profile = profile
    )
  }

  construct_options(opts)
}


check_options <- function(opts) {
  opts_check <- is_bad_option(opts)

  if (!all(opts_check)) {
    option_names <- as.list(names(opts_check)[!opts_check])
    names(option_names) <- "*"
    cli::cli_warn(do.call(c, c(
      paste(
        "The following {length(option_names)} option{?s} ",
        "{?is/are} formatted incorrectly and will be",
        "omitted:"
      ),
      option_names
    )))
  }
}


format_ors_list <- function(features, matches, profile = NULL, single = FALSE) {
  if (isTRUE(features)) {
    features <- matches
  }

  if (!length(features)) {
    features <- NULL
  }

  features <- match.arg(features, matches, several.ok = !single)

  if (!is.null(profile)) {
    opts_check <- names(profile) == profile
  } else {
    opts_check <- TRUE
  }

  structure(features, opts_check = opts_check)
}


format_ors_vector <- function(val,
                              type = "tf",
                              profile = NULL,
                              box = FALSE,
                              single = FALSE) {
  opts_check <- FALSE

  if (type == "tf") {
    fun_check <- \(x) isTRUE(x) || isFALSE(x)
  } else {
    fun_check <- match.fun(paste0("is.", type))
  }

  if (box) {
    val <- list(val)
  }

  profile_check <- if (!is.null(profile)) {
    names(profile) == profile
  } else {
    TRUE
  }

  length_check <- if (single) {
    length(val)
  } else {
    TRUE
  }

  opts_check <- profile_check && length_check && fun_check(val)

  structure(val, opts_check = opts_check)
}


format_ors_poly <- function(poly) {
  if (is_sf(poly) && sf::st_is(poly, c("POLYGON", "MULTIPOLYGON"))) {
    geom_type <- as.character(sf::st_geometry_type(opts$avoid_polygons))
    poly <- list(
      type = capitalize_char(geom_type),
      coordinates = list(sf::st_coordinates(opts$avoid_polygons))
    )
    opts_check <- TRUE
  } else {
    opts_check <- FALSE
  }

  structure(poly, opts_check = opts_check)
}


format_ors_profile_params <- function(params, which, profile = NULL) {
  if (is.vector(params) && !identical(profile, "driving-car")) {
    base_profile <- base_profile(profile)

    defined <- list(
      restrictions = list(
        wheelchair = c(
          "maximum_incline", "maximum_sloped_kerb", "minimum_width",
          "smoothness_type", "surface_type", "track_type"),
        driving = c(
          "axleload", "hazmat", "height", "length", "weight", "width"
        ),
        cycling = "gradient"
      ),
      weightings = list(
        cycling = "steepness_difficulty",
        walking = c("green", "quiet")
      )
    )

    defined <- defined[[which]][[base_profile]]
    admitted <- defined[match(names(params), defined)]
    if (is.null(admitted)) admitted <- list()
    opts_check <- all(names(params) %in% admitted)
  } else {
    admitted <- list()
    opts_check <- FALSE
  }

  structure(admitted, opts_check = opts_check)
}


ready_to_sent <- function(opts) {
  lengths(opts) > 0L & is_bad_option(opts)
}


is_bad_option <- function(opts) {
  vapply(opts, function(x) {
    if (!is.null(x)) attr(x, "opts_check") else TRUE
  }, FUN.VALUE = logical(1))
}


construct_options <- function(opts) {
  profile_params <- list(
    allow_unsuitable      = opts$allow_unsuitable,
    surface_quality_known = opts$surface_quality_known,
    restrictions          = opts$restrictions,
    weightings            = opts$weightings
  )
  profile_params <- profile_params[ready_to_sent(profile_params)]
  attr(profile_params, "opts_check") <- TRUE

  adv_opts <- list(
    avoid_borders   = opts$avoid_borders,
    avoid_countries = opts$avoid_countries,
    avoid_features  = opts$avoid_features,
    avoid_polygons  = opts$avoid_polygons,
    profile_params  = profile_params,
    vehicle_type    = opts$vehicle_type
  )
  adv_opts <- adv_opts[ready_to_sent(adv_opts)]
  attr(adv_opts, "opts_check") <- TRUE

  opts_list <- list(
    attributes        = opts$attributes,
    continue_straight = opts$continue_straight,
    elevation         = opts$elevation,
    extra_info        = opts$extra_info,
    geometry_simplify = opts$geometry_simplify,
    opts              = adv_opts,
    preference        = opts$preference,
    radiuses          = opts$radiuses,
    maximum_speed     = opts$maximum_speed
  )
  opts_list <- opts_list[ready_to_sent(opts_list)]

  opts_list
}
