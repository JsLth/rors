#' Checks and changes the format of input data to all routing functions
#' @param to_coords Whether to convert input sf dataframes to normal dataframes
#' holding coordinates
#' @noRd
format_input_data <- function(.data, to_coords = TRUE) {
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


#' Formats ORS options, checks if they're valid and constructs a list that
#' can be used to create an http query
#' @noRd
format_ors_options <- function(opts, profile) {
  if (is.null(opts)) return(NULL)

  for (opt in names(opts)) {
    params <- as.list(known_opts[known_opts$name == opt, ])
    params$profile <- eval(str2lang(as.character(params$profile)))
    opt_fun <- match.fun(paste0("format_ors_", params$fun))
    opts[[opt]] <- do.call(opt_fun, c(
      list(x = opts[[opt]]),
      list(which = opt),
      params
    ))
  }

  check_options(opts)
  construct_options(opts)
}


#' Takes an ORS options list that contains attributes on whether the respective
#' option is formatted correctly and throws a warning if not.
#' @noRd
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


#' Format and check ORS list parameters
#' @noRd
format_ors_list <- function(x, matches, which, profile, single, ...) {
  matches <- unlist(matches)

  if (isTRUE(x)) {
    allowed <- extra_info_profiles$profiles %in% base_profile(profile) |
      is.na(extra_info_profiles$profiles)
    x <- matches[allowed]
  }

  if (!length(x)) {
    x <- NULL
  }

  x <- matches[match(x, unlist(matches), nomatch = 0L)]

  if (!is.na(profile) && !is.null(names(profile))) {
    opts_check <- names(profile) == profile
  } else {
    opts_check <- TRUE
  }
  
  if (length(x) > 1 && single) {
    opts_check <- FALSE
  }

  structure(x, opts_check = opts_check)
}


#' Format and check ORS scalar parameters
#' @noRd
format_ors_vector <- function(x, type, profile, box, single, ...) {
  opts_check <- FALSE

  if (type == "logical") {
    fun_check <- \(x) isTRUE(x) || isFALSE(x)
  } else {
    fun_check <- match.fun(paste0("is.", type))
  }

  if (box) {
    x <- list(x)
  }

  profile_check <- if (!is.na(profile)) {
    names(profile) == profile
  } else {
    TRUE
  }

  length_check <- if (single) {
    length(x) == 1
  } else {
    TRUE
  }

  opts_check <- profile_check && length_check && fun_check(x)

  structure(x, opts_check = opts_check)
}


#' Format and check ORS geojson
#' @noRd
format_ors_poly <- function(x, ...) {
  if (is_sf(poly) && sf::st_is(poly, c("POLYGON", "MULTIPOLYGON"))) {
    geom_type <- as.character(sf::st_geometry_type(x))
    poly <- list(
      type = capitalize_char(geom_type),
      coordinates = list(sf::st_coordinates(x))
    )
    opts_check <- TRUE
  } else {
    opts_check <- FALSE
  }

  structure(poly, opts_check = opts_check)
}


#' Format and check ORS profile parameters
#' @noRd
format_ors_profile_params <- function(x, which, profile, ...) {
  x <- as.list(x)
  
  if (is.vector(x) && !identical(profile, "driving-car")) {
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
    admitted <- defined[match(names(x), defined)]
    x <- x[admitted]
    if (is.null(admitted)) admitted <- list()
    opts_check <- all(names(x) %in% admitted)
  } else {
    admitted <- list()
    opts_check <- FALSE
  }

  structure(admitted, opts_check = opts_check)
}


#' Checks if an option is ready to be sent in an http request, i.e. is an
#' option not empty and not formatted wrongly?
#' @noRd
ready_to_sent <- function(opts) {
  lengths(opts) > 0L & is_bad_option(opts)
}


#' Checks if an option does not have a FALSE opts_check attribute
#' @noRd
is_bad_option <- function(opts) {
  vapply(opts, function(x) {
    if (!is.null(x)) attr(x, "opts_check") else TRUE
  }, FUN.VALUE = logical(1))
}


#' Constructs an ORS options list that is ready to be sent in a http request
#' @noRd
construct_options <- function(opts) {
  profile_params <- opts[c(
    "allow_unsuitable", "surface_quality_known", "restrictions", "weightings"
  )]
  profile_params <- profile_params[ready_to_sent(profile_params)]
  attr(profile_params, "opts_check") <- TRUE

  adv_opts <- opts[c(
    "avoid_borders", "avoid_countries", "avoid_features",
    "avoid_polygons", "vehicle_type"
  )]
  adv_opts[["profile_params"]] <- profile_params
  adv_opts <- adv_opts[ready_to_sent(adv_opts)]
  attr(adv_opts, "opts_check") <- TRUE

  opts_list <- opts[c(
    "attributes", "continue_straight", "elevation", "extra_info",
    "geometry_simplify", "preference", "radiuses", "maximum_speed"
  )]
  opts_list[["options"]] <- adv_opts
  opts_list <- opts_list[ready_to_sent(opts_list)]

  opts_list
}


#' A tibble that specifies which extra info is defined for which profile.
#' `NA` means allowed for all profiles.
#' @noRd
extra_info_profiles <- tibble::tibble(
  name = c(
    "steepness", "suitability", "surface", "waycategory", "waytype", "tollways",
    "traildifficulty", "osmid", "roadaccessrestrictions", "countryinfo",
    "green", "noise"
  ),
  profiles = c(
    NA, NA, NA, NA, NA, "driving", NA, "wheelchair", "driving",
    "driving", "foot", "foot"
  )
)


#' A tibble that specifies the way each ORS option is to be formatted.
#' `fun` holds the formatting function that each option is passed on to.
#' `matches` holds a list of defined values for respective options
#' `single` specifies whether an option must be length 1.
#' `type` specifies the data type of an option
#' `profile` holds an unevaluated named character that specifies both required
#' profile name and provided profile name (after evaluation)
#' `box` specifies whether to box an option for json conversion
#' 
#' A value of NA means that the formatting parameter is not relevant for a
#' given option.
#' @noRd
known_opts <- tibble::tibble(
  name = c(
    "geometry_simplify", "continue_straight", "avoid_borders",
    "avoid_countries", "avoid_features", "avoid_polygons", "restrictions",
    "weightings", "allow_unsuitable", "surface_quality_known", "vehicle_type",
    "preference", "radiuses", "maximum_speed", "attributes", "extra_info",
    "elevation"
  ),
  fun = c(
    "vector", "vector", "list", "vector", "vector", "poly", "profile_params",
    "profile_params", "vector", "vector", "vector", "list", "vector",
    "vector", "list", "list", "vector"
  ),
  matches = list(
    NULL, NULL, c("all", "controlled", "none"), NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, c("fastest", "shortest", "recommended"), NULL,
    NULL, c("avgspeed", "detourfactor"), extra_info_profiles$name, NULL
  ),
  single = c(
    TRUE, TRUE, TRUE, FALSE, FALSE, NA, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, FALSE, FALSE, TRUE
  ),
  type = c(
    "logical", "logical", NA, "numeric", "character", NA, NA, NA,
    "logical", "logical", "character", NA, "numeric", "numeric", NA,
    NA, "logical"
  ),
  profile = c(
    NA, NA, 'c(driving = base_profile("profile"))',
    'c(driving = base_profile(profile))', NA, NA, 'profile', 'profile',
    'c("wheelchair" = profile)', 'c("wheelchair" = profile)',
    'c("driving-hgv" = profile)', NA, NA, NA, NA, 'profile', NA
  ),
  box = c(
    FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  )
)
