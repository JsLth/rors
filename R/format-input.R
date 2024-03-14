#' Checks and changes the format of input data to all routing functions
#' @param to_coords Whether to convert input sf dataframes to normal dataframes
#' holding coordinates
#' @noRd
prepare_input <- function(input, to_coords = TRUE, len = NULL) {
  assert_that(is_geometry_type(input, "POINT"))
  input <- sf::st_geometry(input)

  if (has_crs(input)) {
    input <- sf::st_transform(input, 4326)
  } else {
    input <- sf::st_set_crs(input, 4326)
  }

  if (to_coords) {
    input <- sf::st_coordinates(input)[, c("X", "Y"), drop = FALSE]
  }

  if (!is.null(len)) {
    if (nrow(input) == 1) {
      input <- replicate(len, input, simplify = FALSE)
      input <- do.call(rbind, input)
    }

    if (!nrow(input) == len) {
      cli::cli_abort(c(
        "x" = "Datasets have non-matching number of rows.",
        "!" = paste(
          "{.var src} and {.var src} must have either one row",
          "or the number of rows of the other dataset."
        ),
        "i" = "Got datasets with {.val {nrow(input)}} and {.val {len}} rows."
      ))
    }
  }

  input
}


#' Formats ORS options, checks if they're valid and constructs a list that
#' can be used to create an http query
#' @noRd
prepare_ors_params <- function(params, profile) {
  params %||% return()
  params <- format_ors_params(params, profile)
  check_ors_params(params)
  construct_ors_params(params)
}


format_ors_params <- function(params, profile) {
  validate_ors_dots(params)
  info <- param_info()
  pnames <- names(params)

  params <- lapply(pnames, function(param) {
    val <- params[[param]]

    if (inherits(val, "list")) {
      format_ors_params(val, profile)
    } else {
      info <- as.list(info[info$name == param, ])
      args <- list(x = val, param = param, profile = profile)
      do.call(param_prepare, c(args, info))
    }
  })
  names(params) <- pnames
  params
}


#' Format and check ORS vector parameters
#' @noRd
param_prepare <- function(x,
                          param,
                          profile,
                          type,
                          box,
                          scalar,
                          match,
                          poly,
                          allowed,
                          ...) {
  checks <- c("type", "profile", "scalar", "match", "poly")
  names(checks) <- checks

  checks <- vswitch(
    checks,
    FUN.VALUE = logical(1),
    USE.NAMES = TRUE,
    type = param_check_type(x, type),
    profile = param_check_profile(profile, allowed),
    scalar = param_check_scalar(x, scalar),
    match = param_check_match(x, param, match, profile, allowed),
    poly = param_check_poly(x, poly)
  )

  if (all(checks)) {
    x <- param_format_poly(x, poly)
    x <- param_format_boxed(x, box)
  }

  attr(x, "check") <- param_verify(checks, param)
  x
}


param_check_type <- function(x, type) {
  switch(
    type,
    logical = is_true_or_false(x),
    double = is.double(x),
    integer = is.numeric(x) && all(as.integer(x) == x),
    character = is.character(x),
    TRUE
  )
}


param_check_profile <- function(profile, allowed) {
  startsWith(profile, allowed %NA% profile)
}


param_check_scalar <- function(x, scalar) {
  cmp_fun <- if (isTRUE(scalar)) `==` else `>=`
  cmp_fun(length(x), 1)
}


param_check_match <- function(x, name, match, profile, allowed) {
  if (!match) return(TRUE)

  if (identical(name, "extra_info")) {
    allowed <- startsWith(profile, param_extra_info_allowed(x))
    x[!allowed] <- NA
  }

  identical(x, param_match_arg(name, x))
}


param_check_poly <- function(x, poly) {
  if (poly) {
    is_sf(x) && all(sf::st_is(x, c("POLYGON", "MULTIPOLYGON")))
  } else {
    TRUE
  }
}


param_format_boxed <- function(x, box) {
  if (box) list(x)
  x
}


param_format_poly <- function(x, poly) {
  if (poly) {
    tempf <- tempfile(fileext = ".geojson")
    on.exit(unlink(tempf))
    capture.output(
      sf::st_write(sf::st_geometry(x), tempf),
      type = "output"
    )
    x <- jsonlite::read_json(tempf)
  }
  x
}


param_verify <- function(check, name) {
  warn <- names(check)[which(!check)]
  all_ok <- all(check)

  if (!all_ok) {
    vswitch(
      warn,
      type = "invalid type",
      profile = "requires a different profile",
      scalar =  "invalid length",
      match = paste0(
        "undefined values",
        if (identical(name, "extra_info")) " (possibly wrong profile)"
      ),
      poly = "invalid sf object"
    )
  }
}


validate_ors_dots <- function(params) {
  params_ok <- names(params) %in% param_info()$name
  if (!all(params_ok)) {
    cli::cli_abort(
      "Unknown ORS option{?s} {.var {names(params[!params_ok])}}."
    )
  }

  params_dup <- duplicated(names(params))
  if (any(params_dup)) {
    cli::cli_abort(
      "Duplicated ORS option{?s} {.var {names(params[params_dup])}}"
    )
  }
}


check_ors_params <- function(params) {
  checks <- drop_null(lapply(flatten_list(params), attr, "check"))

  if (length(checks)) {
    reasons <- vapply(checks, paste, character(1), collapse = ", ")
    bad_params <- paste(
      cli::col_blue(cli::symbol$bullet),
      sprintf("{.field {'%s'}}:", names(checks)),
      reasons
    )
    n_bad <- length(bad_params)
    names(bad_params) <- rep(" ", n_bad)

    cli::cli_abort(c(
      paste(
        "The following {cli::qty(n_bad)} parameter{?s}",
        "{?is/are} invalid and need{?s/} to be revised"
      ),
      bad_params
    ))
  }
}


#' Checks if an option is ready to be sent in an http request, i.e. is an
#' option not empty and not formatted wrongly?
#' @noRd
ready_to_sent <- function(params) {
  lengths(params) > 0L & is_bad_param(params)
}


#' Checks if an option does not have a FALSE opts_check attribute
#' @noRd
is_bad_param <- function(params) {
  vapply(opts, function(x) {
    if (!is.null(x)) attr(x, "opts_check") else TRUE
  }, FUN.VALUE = logical(1))
}


construct_ors_object <- function(obj, name, params, info) {
  parent <- info[info$name %in% name, "parent"]
  if (!is.na(parent)) {
    obj <- list(obj)
    names(obj) <- name
    construct_ors_object(obj, parent, params, info)
  } else {
    params[[name]] <- obj
    params
  }
}


#' Constructs an ORS options list that is ready to be sent in a http request
#' @noRd
construct_ors_params <- function(params) {
  pnames <- names(params)
  info <- param_info()
  param_list <- list()

  for (name in pnames) {
    param_list <- construct_ors_object(params[[name]], name, param_list, info)
  }

  invisible(params)
}


#' A tibble that specifies which extra info is defined for which profile.
#' `NA` means allowed for all profiles.
#' @noRd
param_extra_info_allowed <- function(name) {
  vswitch(
    name,
    tollways = "driving",
    osmid = "wheelchair",
    roadaccessrestrictions = "driving",
    countryinfo = "driving",
    green = "foot",
    noise = "foot",
    shadow = "foot",
    ""
  )
}


#' A tibble that specifies the way each ORS option is to be formatted.
#' `fun` holds the formatting function that each option is passed on to.
#' `matches` holds a list of defined values for respective options
#' `scalar` specifies whether an option must be length 1.
#' `type` specifies the data type of an option
#' `allowed` specifies allowed profiles for this parameter
#' `box` specifies whether to box an option for json conversion
#'
#' NA means that the formatting parameter is not relevant for a given option.
#'
#' constructive::construct(dplyr::arrange(
#'   param_info(),
#'   match(parent, c(
#'     NA, "options", "profile_params", "restrictions", "weightings",
#'     "round_trip", "alternative_routes"
#'   ))
#' ), constructive::opts_atomic(compress = FALSE))
#'
#' @noRd
param_info <- function(names = NULL) {
  data.frame(
    name = c(
      "options", "bearings", "alternative_routes", "geometry_simplify",
      "continue_straight", "preference", "radiuses", "maximum_speed", "attributes",
      "extra_info", "elevation", "profile_params", "avoid_borders",
      "avoid_countries", "avoid_features", "avoid_polygons", "round_trip",
      "vehicle_type", "restrictions", "weightings", "allow_unsuitable",
      "surface_quality_known", "length", "width", "height", "axleload", "weight",
      "hazmat", "surface_type", "track_type", "smoothness_type",
      "maximum_sloped_kerb", "maximum_incline", "minimum_width",
      "steepness_difficulty", "green", "quiet", "shadow", "length", "points",
      "seed", "target_count", "weight_factor", "share_factor"
    ),
    parent = c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "options", "options", "options",
      "options", "options", "options", "options", "profile_params",
      "profile_params", "profile_params", "profile_params", "restrictions",
      "restrictions", "restrictions", "restrictions", "restrictions",
      "restrictions", "restrictions", "restrictions", "restrictions",
      "restrictions", "restrictions", "restrictions", "weightings", "weightings",
      "weightings", "weightings", "round_trip", "round_trip", "round_trip",
      "alternative_routes", "alternative_routes", "alternative_routes"
    ),
    match = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
      TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    scalar = c(
      NA, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,
      NA, TRUE, FALSE, FALSE, NA, NA, TRUE, NA, NA, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    ),
    type = c(
      NA, NA, NA, "logical", "logical", "character", "numeric", "numeric",
      "character", "character", "logical", NA, "character", "numeric",
      "character", NA, NA, "character", NA, NA, "logical", "logical", "double",
      "double", "double", "double", "double", "logical", "character",
      "character", "character", "double", "integer", "double", "integer",
      "double", "double", "double", "double", "integer", "integer", "integer",
      "double", "double"
    ),
    allowed = c(
      NA, "cycling", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "driving", "driving",
      NA, NA, NA, "driving-hgv", NA, NA, "wheelchair", "wheelchair", "driving-hgv",
      "driving-hgv", "driving-hgv", "driving-hgv", "driving-hgv", "driving-hgv",
      "wheelchair", "wheelchair", "wheelchair", "wheelchair", "wheelchair",
      "wheelchair", "cycling", "foot", "foot", "foot", NA, NA, NA, NA, NA, NA
    ),
    poly = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    box = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    inspect = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE
    ),
    pairwise = c(
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE
    ),
    accessibility = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE
    )
  )
}


param_match_arg <- function(name, values) {
  table <- switch(
    name,
    avoid_borders = c("all", "controlled", "none"),
    preference = c("fastest", "shortest", "recommended"),
    attributes = c("avgspeed", "detourfactor", "percentage"),
    extra_info = c(
      "steepness", "suitability", "surface", "waycategory", "waytype",
      "tollways", "traildifficulty", "osmid", "roadaccessrestrictions",
      "countryinfo", "green", "noise"
    ),
    vehicle_type = c(
      "hgv", "bus", "agricultural", "delivery",
      "forestry", "goods", "unknown"
    ),
    track_type = c("grade1", "grade2", "grade3", "grade4", "grade5"),
    smoothness_type = c(
      "excellent", "good", "intermediate", "bad", "very_bad",
      "horrible", "very_horrible", "impassable"
    ),
    steepness_difficulty = 0:3
  )

  table[match(values, table, nomatch = 0)]
}
