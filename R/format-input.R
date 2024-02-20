#' Checks and changes the format of input data to all routing functions
#' @param to_coords Whether to convert input sf dataframes to normal dataframes
#' holding coordinates
#' @noRd
prepare_input <- function(.data, to_coords = TRUE, len = NULL) {
  assert_that(is_geometry_type(.data, "POINT"))
  .data <- sf::st_geometry(.data)

  if (has_crs(.data)) {
    .data <- sf::st_transform(.data, 4326)
  } else {
    .data <- sf::st_set_crs(.data, 4326)
  }

  if (to_coords) {
    .data <- sf::st_coordinates(.data)[, c("X", "Y"), drop = FALSE]
  }

  if (!is.null(len)) {
    if (nrow(.data) == 1) {
      .data <- replicate(len, .data, simplify = FALSE)
      .data <- do.call(rbind, .data)
    }

    if (!nrow(.data) == len) {
      cli::cli_abort(c(
        "x" = "Datasets have non-matching number of rows.",
        "!" = paste(
          "{.var src} and {.var src} must have either one row",
          "or the number of rows of the other dataset."
        ),
        "i" = "Got datasets with {.val {nrow(.data)}} and {.val {len}} rows."
      ))
    }
  }

  .data
}


#' Formats ORS options, checks if they're valid and constructs a list that
#' can be used to create an http query
#' @noRd
format_ors_params <- function(opts, profile) {
  if (is.null(opts)) return(NULL)

  validate_ors_dots(opts)

  for (opt in names(opts)) {
    params <- as.list(known_params[known_params$name == opt, ])
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
format_ors_list <- function(x, matches, which, profile, scalar, ...) {
  matches <- unlist(matches)

  if (isTRUE(x)) {
    if (which == "extra_info") {
      allowed <- extra_info_profiles$profiles %in% base_profile(profile) |
        is.na(extra_info_profiles$profiles)
    } else {
      allowed <- seq_along(matches)
    }

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

  if (length(x) > 1 && scalar) {
    opts_check <- FALSE
  }

  structure(x, opts_check = opts_check)
}


#' Format and check ORS scalar parameters
#' @noRd
format_ors_vector <- function(x, type, profile, box, scalar, ...) {
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

  length_check <- if (scalar) {
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
format_ors_nlist <- function(x, which, profile, ...) {
  x <- as.list(x)

  if (is.recursive(x) && (!identical(profile, "driving-car")) || is.na(profile)) {
    defined <- nlist_params[[which]]
    if (!is.na(profile)) defined <- defined[[base_profile(profile)]]
    admitted <- defined[match(names(x), defined)]
    x <- x[admitted]
    opts_check <- length(x) > 0
  } else {
    x <- list()
    opts_check <- FALSE
  }

  structure(x, opts_check = opts_check)
}

validate_ors_dots <- function(opts) {
  opts_ok <- names(opts) %in% known_params$name
  if (!all(opts_ok)) {
    cli::cli_abort(c(
      "x" = "Unknown ORS option{?s} {.var {names(opts[!opts_ok])}}."
    ))
  }

  opts_dup <- duplicated(names(opts))
  if (any(opts_dup)) {
    cli::cli_abort(c(
      "x" = "Duplicated ORS option{?s} {.var {names(opts[opts_dup])}}"
    ))
  }
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
    "avoid_polygons", "vehicle_type", "round_trip"
  )]
  adv_opts[["profile_params"]] <- profile_params
  adv_opts <- adv_opts[ready_to_sent(adv_opts)]
  attr(adv_opts, "opts_check") <- TRUE

  opts_list <- opts[c(
    "alternative_routes", "attributes", "continue_straight", "elevation",
    "extra_info", "geometry_simplify", "preference", "radiuses", "maximum_speed"
  )]
  opts_list[["options"]] <- adv_opts
  opts_list <- opts_list[ready_to_sent(opts_list)]

  opts_list
}


#' A tibble that specifies which extra info is defined for which profile.
#' `NA` means allowed for all profiles.
#' @noRd
extra_info_profiles <- data.frame(
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
#' `scalar` specifies whether an option must be length 1.
#' `type` specifies the data type of an option
#' `profile` holds an unevaluated named character that specifies both required
#' profile name and provided profile name (after evaluation)
#' `box` specifies whether to box an option for json conversion
#'
#' NA means that the formatting parameter is not relevant for a given option.
#' @noRd
known_params <- structure(
  list(
    name = c(
      "alternative_routes", "geometry_simplify", "continue_straight",
      "avoid_borders", "avoid_countries", "avoid_features", "avoid_polygons",
      "round_trip", "restrictions", "weightings", "allow_unsuitable",
      "surface_quality_known", "vehicle_type", "preference", "radiuses",
      "maximum_speed", "attributes", "extra_info", "elevation"
    ),
    fun = c(
      "nlist", "vector", "vector", "list", "vector", "vector", "poly", "nlist",
      "nlist", "nlist", "vector", "vector", "vector", "list", "vector", "vector",
      "list", "list", "vector"
    ),
    matches = list(
      NULL,
      NULL,
      NULL,
      c("all", "controlled", "none"),
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      c("fastest", "shortest", "recommended"),
      NULL,
      NULL,
      c("avgspeed", "detourfactor", "percentage"),
      c(
        "steepness", "suitability", "surface", "waycategory", "waytype", "tollways",
        "traildifficulty", "osmid", "roadaccessrestrictions", "countryinfo", "green",
        "noise"
      ),
      NULL
    ),
    scalar = rep(c(NA, TRUE, FALSE, NA, TRUE, FALSE, TRUE), c(1L, 3L, 2L, 4L, 6L, 2L, 1L)),
    type = c(
      NA, "logical", "logical", NA, "numeric", "character", NA, NA, NA, NA,
      "logical", "logical", "character", NA, "numeric", "numeric", NA, NA,
      "logical"
    ),
    profile = c(
      NA, NA, NA, "c(driving = base_profile(\"profile\"))",
      "c(driving = base_profile(profile))", NA, NA, NA, "profile", "profile",
      "c(\"wheelchair\" = profile)", "c(\"wheelchair\" = profile)",
      "c(\"driving-hgv\" = profile)", NA, NA, NA, NA, "profile", NA
    ),
    box = rep(c(FALSE, TRUE, FALSE, TRUE, FALSE), c(4L, 2L, 6L, 1L, 6L))
  ),
  class = "data.frame",
  row.names = 1:19
)


nlist_params <- list(
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
    foot = c("green", "quiet")
  ),
  alternative_routes = c("share_factor", "target_count", "weight_factor"),
  round_trip = c("length", "points", "seed")
)
