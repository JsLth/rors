#' Construct ORS parameters
#'
#' @description
#' Create a list of parameters that can be passed to a function that supports
#' additional parameters. For further information on parameters, visit the
#' \href{https://openrouteservice.org/dev/#/api-docs/v2}{API playground} or
#' the \href{https://giscience.github.io/openrouteservice/api-reference/endpoints/directions/routing-options}{backend reference}.
#'
#' Note that this function provides a means to manually construct parameters.
#' For all high-level endpoint functions such as \code{\link{ors_inspect}},
#' parameters can be passed as dot arguments.
#'
#' @param profile \code{[character]}
#'
#' A routing profile defined by ORS. Not all combinations of
#' profiles and parameters are supported.
#' @param bearings \code{[numeric]}
#'
#' Numeric vector or matrix with \code{n} rows and 1 or 2 columns. The first
#' column contains bearings that can take radial degrees between 0 and 360
#' (clockwise) that represent the direction during routing. An optional
#' second column specifies the possible deviation from the bearings. Both
#' values can also be \code{NA_real_} to skip a value or an entire input
#' row. Bearings default to 100 if not specified. Only available for
#' \code{cycling-*} profiles.
#'
#' @param geometry_simplify \code{[logical]}
#'
#' Whether to simplify the route geometry. Only possible if
#' \code{length(n) == 1}. Not available if \code{extra_info}s are specified.
#'
#' @param continue_straight \code{[logical]}
#'
#' Whether to force the route to go straight and avoid u-turns.
#'
#' @param preference \code{[character]}
#'
#' Specifies the route preference. Must be once of \code{"fastest"},
#' \code{"shortest"} or \code{"recommended"}. Defaults to \code{"recommended"}.
#'
#' @param radiuses \code{[numeric]}
#'
#' Maximum snapping distance (in m) from an input point to the
#' nearest road. Expects a vector of length \code{n}. If a vector of length 1
#' is provided, values are recycled to length \code{n}. \code{-1} represents
#' an unlimited radius. Defaults to \code{maximum_snapping_radius} in the
#' configuration or 350m if not changed.
#'
#' @param maximum_speed \code{[numeric]}
#'
#' Maximum allowed speed in km/h. Must be at least 80 km/h. Only available
#' for \code{driving-*} profiles.
#'
#' @param attributes \code{[character]}
#'
#' Route attributes to be sent with each routing result. One or several of
#' \code{"avgspeed"}, \code{"detourfactor"} or \code{"percentage"}.
#'
#' @param extra_info \code{[character]}
#'
#' Extra information to be sent with each routing result. For a list of
#' possible values, see the details section in \code{\link{ors_inspect}}.
#' Because \code{extra_info} changes the output format, it is only available for
#' \code{\link{ors_inspect}}.
#'
#' @param elevation \code{[logical]}
#'
#' Whether to include elevation information in the routing response.
#'
#' @param skip_segments \code{[numeric]}
#'
#' Vector of route segments to skip. Can contain one or multiple values
#' representing segments (i.e. indices of input rows) that should be skipped
#' in route computations. The vector cannot have more segments than the
#' number of rows in \code{src}.
#'
#' @param roundabout_exits \code{[logical]}
#'
#' Whether to include information about bearings of roundabout exits in
#' the routing response.
#'
#' @param maneuvers \code{[logical]}
#'
#' Whether to include information about maneuvers in the routing response.
#' Maneuvers describe the bearing before and after a waypoint has been
#' passed on a route.
#'
#' @param suppress_warnings \code{[logical]}
#'
#' Whether to suppress warnings in the routing response.
#'
#' @param id \code{[character]}
#'
#' Any identifier to be sent with the routing request and returned in the
#' routing response.
#'
#' @param instructions \code{[logical]}
#'
#' Whether to include navigation information in the routing response.
#'
#' @param instructions_format \code{[character]}
#'
#' Format for navigation instructions. Can be one of \code{"text"} or
#' \code{"html"}. HTML instructions are more verbose.
#'
#' @param language \code{[character]}
#'
#' Language of the navigation instructions. One of \code{"cs"}, \code{"cs-cz"},
#' \code{"de"}, \code{"de-de"}, \code{"en"}, \code{"en-us"}, \code{"eo"},
#' \code{"eo-eo"}, \code{"es"}, \code{"es-es"}, \code{"fr"}, \code{"fr-fr"},
#' \code{"gr"}, \code{"gr-gr"}, \code{"he"}, \code{"he-il"}, \code{"hu"},
#' \code{"hu-hu"}, \code{"id"}, \code{"id-id"}, \code{"it"}, \code{"it-it"},
#' \code{"ja"}, \code{"ja-jp"}, \code{"ne"}, \code{"ne-np"}, \code{"nl"},
#' \code{"nl-nl"}, \code{"nb"}, \code{"nb-no"}, \code{"pl"}, \code{"pl-pl"},
#' \code{"pt"}, \code{"pt-pt"}, \code{"ro"}, \code{"ro-ro"}, \code{"ru"},
#' \code{"ru-ru"}, \code{"tr"}, \code{"tr-tr"}, \code{"zh"} or \code{"zh-cn"}.
#'
#' @param avoid_borders \code{[character]}
#'
#' Type of border to avoid. One of \code{"all"}, \code{"controlled"},
#' or \code{"none"}. Requires \code{driving-*} profiles.
#'
#' @param avoid_countries \code{[integer]}
#'
#' Vector of country codes to avoid. Requires
#' \code{avoid_borders = "controlled"}. A list of country codes can be
#' found in the
#' \href{https://giscience.github.io/openrouteservice/technical-details/country-list}{API reference}
#' or by running \code{\link[=info_table]{info_table("country_list")}}.
#' Requires \code{driving-*} profiles.
#'
#' @param avoid_features \code{[character]}
#'
#' Features to avoid. One of \code{"highways"}, \code{"tollways"} or
#' \code{"ferries"}.
#'
#' @param avoid_polygons \code{[sf/sfc]}
#'
#' \code{sf} object containing polygons or multipolygons that describe areas to
#' avoid. Must have CRS \code{EPSG:4326}.
#'
#' @param vehicle_type \code{[character]}
#'
#' If \code{profile} is \code{"driving-hgv"}, specifies the vehicle type.
#' One of \code{"hgv"}, \code{"bus"}, \code{"agricultural"},
#' \code{"agricultural"}, \code{"delivery"}, \code{"forestry"}, \code{"goods"}
#' or \code{"unknown"}.
#'
#' @param restrictions \code{[list]}
#'
#' List of options for restricting waypoint edges. If an edge does not
#' meet the restrictions, it is discarded. Can include the following options:
#'
#' \describe{
#'  \item{\code{length}}{Length restrictions for \code{"driving-hgv"} in m.}
#'  \item{\code{width}}{Width restrictions for \code{"driving-hgv"} in m.}
#'  \item{\code{height}}{Height restrictions for \code{"driving-hgv"} in m.}
#'  \item{\code{axleload}}{Axleload restrictions for \code{"driving-hgv"} in tons.}
#'  \item{\code{weight}}{Weight restrictions for \code{"driving-hgv"} in m.}
#'  \item{\code{hazmat}}{Whether to adjust routing for transportation of
#'  hazardous goods, i.e. avoid protected areas, for \code{"driving-hgv"}.
#'  Defaults to \code{FALSE}.}
#'  \item{\code{surface_type}}{Minimum surface type for \code{"wheelchair"}.
#'  Corresponds to the values of OSM
#'  \href{https://wiki.openstreetmap.org/wiki/Key:surface}{Key:surface}.
#'  Defaults to \code{"sett"}.}
#'  \item{\code{track_type}}{Minimum track grade for \code{"wheelchair"}.
#'  Corresponds to the values of OSM
#'  \href{https://wiki.openstreetmap.org/wiki/Key:tracktype}{Key:tracktype}.
#'  Defaults to \code{"grade1"}.}
#'  \item{\code{smoothness_type}}{Minimum track smoothness for \code{"wheelchair"}.
#'  Corresponds to the values of OSM
#'  \href{https://wiki.openstreetmap.org/wiki/Key:smoothness}{Key:smoothness}.
#'  Defaults to \code{"good"}.}
#'  \item{\code{maximum_sloped_kerb}}{Maximum sloped curb height for
#'  \code{"wheelchair"} (in m). Defaults to 0.6 m.}
#'  \item{\code{maximum_incline}}{Maximum incline for \code{"wheelchair"} (in percent).
#'  Defaults to 6 percent.}
#'  \item{\code{minimum_width}}{Minimum footway width for \code{"wheelchair"} (in m).
#'  Defaults to 2.5 m.}
#' }
#'
#' @param weightings \code{[list]}
#'
#' List of options for weighting waypoint edges. Can include the following
#' options:
#'
#' \describe{
#'  \item{steepness_difficulty}{Proficiency level for \code{cycling-*}
#'  profiles. Can be an integer between 0 and 3 corresponding to the
#'  levels "novice", "moderate", "amateuer" and "pro".}
#'  \item{green}{Multiplier between 0 and 1 that weights the importance
#'  of green areas at waypoint edges. If 1, always prefers green routes.
#'  Only available for \code{foot-*} profiles.}
#'  \item{quiet}{Multiplier between 0 and 1 that weights the importance
#'  of quiet areas at waypoint edges. If 1, always prefers quiet routes.
#'  Only available for \code{foot-*} profiles.}
#'  \item{shadow}{Multiplier between 0 and 1 that weights the importance
#'  of shadow areas at waypoint edges. If 1, always prefers shadowy routes.
#'  Only available for \code{foot-*} profiles.}
#' }
#'
#' @param surface_quality_known \code{[logical]}
#'
#' Whether to force routes to follow ways whose surface quality is known.
#' Only available for \code{"wheelchair"} profile.
#'
#' @param allow_unsuitable \code{[logical]}
#'
#' Whether unsuitable ways should be included in routing. Only available
#' for \code{"wheelchair"} profile.
#'
#' @param ... Reserved for further expansion. If a dot argument is used,
#' an error is thrown.
#' @inheritParams ors_pairwise
#' @inheritParams ors_inspect
#'
#' @details
#' This function performs some basic validation checks. If a check fails,
#' an error is thrown. Additionally, if a dot argument is passed, it is
#' interpreted as an unknown parameter and an error is thrown. To allow passing
#' unknown parameters (e.g. if the API has changed), you can set
#' \code{options(rors_allow_unknown_params = TRUE)}
#'
#' @export
#'
#' @examples
#' # set maximum speed and maximum snapping distance
#' ors_params(pharma, "driving-car", maximum_speed = 100, radiuses = -1)
#'
#' # set up a biking route
#' rt_opts <- list(length = 5000, points = 20)
#' ors_params(pharma, "cycling-regular", round_trip = rt_opts)
ors_params <- function(src,
                       profile,
                       bearings = NULL,
                       alternative_routes = list(),
                       geometry_simplify = FALSE,
                       continue_straight = FALSE,
                       preference = "recommended",
                       radiuses = NULL,
                       maximum_speed = NULL,
                       attributes = NULL,
                       extra_info = NULL,
                       elevation = FALSE,
                       skip_segments = NULL,
                       roundabout_exits = FALSE,
                       maneuvers = FALSE,
                       suppress_warnings = FALSE,
                       id = NULL,
                       instructions = TRUE,
                       instructions_format = "text",
                       language = "en",
                       avoid_borders = NULL,
                       avoid_countries = NULL,
                       avoid_features = NULL,
                       avoid_polygons = NULL,
                       round_trip = list(),
                       vehicle_type = "hgv",
                       restrictions = list(),
                       weightings = list(),
                       surface_quality_known = FALSE,
                       allow_unsuitable = FALSE,
                       ...) {
  params <- lapply(match.call()[-1], eval)
  params[c("src", "profile")] <- NULL
  params <- prepare_ors_params(params, src, profile) %||% list()
  structure(
    params,
    class = "ors_params",
    profile = profile,
    n = nrow(src)
  )
}


assert_ors_params <- function(params, src, profile) {
  if (is.null(params)) return()

  if (!inherits(params, "ors_params")) {
    abort(c(
      paste(
        "Argument {.code params} must be an object of",
        "class {.cls ors_params} or NULL."
      ),
      "i" = "You can create a parameter object using {.fn ors_params}."
    ), class = "ors_params_assert")
  }

  given_profile <- attr(params, "profile")
  given_segments <- attr(params, "n")
  same_profile <- identical(given_profile, profile)
  same_segments <- identical(given_segments, nrow(src))
  if (!same_profile || !same_segments) {
    msg <- "Argument {.code params} must be built from the same input data as `src`"
    add <- c(
      "Given profile: {.val {given_profile}} - actual profile: {.val {profile}}",
      "Given segments: {.val {given_segments}} - actual profile: {.val {nrow(src)}}"
    )
    names(add) <- c("*", "*")
    add <- add[c(!same_profile, !same_segments)]
    abort(c(msg, add), class = "ors_params_assert_incompatible")
  }
}


#' Formats ORS options, checks if they're valid and constructs a list that
#' can be used to create an http query
#' @noRd
prepare_ors_params <- function(params, src, profile, endpoint = NULL) {
  params %empty% return()
  validate_param_names(params, endpoint)
  prepare_ors_params_impl(params, profile = profile, n = nrow(src))
}


prepare_ors_params_impl <- function(params, profile, n) {
  for (p in names(params)) {
    val <- params[[p]]
    param_check_profile(profile, p)
    switch(
      p,
      allow_unsuitable = param_check_flag(val, param = p),
      alternative_routes = param_check_alternative_routes(val, profile, param = p),
      avoid_features = param_check_match(val, param = p),
      avoid_borders = param_check_match(val, param = p),
      avoid_countries = param_check_vector(val, c("integer", "double"), param = p),
      avoid_polygons = param_check_poly(val, param = p),
      attributes = param_check_true_or_match(val, param = p),
      bearings = param_check_bearings(val, n = n, param = p),
      continue_straight = param_check_flag(val, param = p),
      elevation = param_check_flag(val, param = p),
      extra_info = param_check_true_or_match(val, param = p),
      geometry_simplify = param_check_flag(val, param = p),
      id = param_check_string(val, param = p),
      instructions = param_check_flag(val, param = p),
      instructions_format = param_check_match(val, param = p),
      language = param_check_match(val, param = p),
      maneuvers = param_check_flag(val, param = p),
      maximum_speed = param_check_number(val, min = 80, param = p),
      preference = param_check_match(val, param = p),
      radiuses = param_check_radiuses(val, n = n, param = p),
      restrictions = param_check_restrictions(val, profile, param = p),
      round_trip = param_check_round_trip(val, profile, param = p),
      roundabout_exits = param_check_flag(val, param = p),
      skip_segments = param_check_number(
        val,
        whole = TRUE,
        multiple = TRUE,
        min = 1,
        max = n,
        param = p
      ),
      suppress_warnings = param_check_flag(val, param = p),
      surface_quality_known = param_check_flag(val, param = p),
      vehicle_type = param_check_match(val, param = p),
      weightings = param_check_weightings(val, profile, param = p),
      consider_unknown_parameter(p)
    )

    # format special parameters
    if (p %in% c("avoid_countries", "avoid_features", "vehicle_type")) {
      params[[p]] <- box(val)
    }

    if (identical(p, "avoid_polygons")) {
      params[[p]] <- sf_to_geojson(val)
    }

    if (identical(p, "bearings")) {
      params[[p]] <- unname(apply(
        as.matrix(val),
        MARGIN = 1,
        function(x) drop_na(unname(c(x))),
        simplify = FALSE
      ))
    }

    if (p %in% c("extra_info", "attributes") && isTRUE(params[[p]])) {
      params[[p]] <- param_lists[[p]]
    }
  }

  # create nested parameters
  params$options$round_trip <- params$round_trip
  params$options$profile_params$weightings <- params$weightings
  params$options$profile_params$restrictions <- params$restrictions
  params[c("round_trip", "weightings", "restrictions")] <- NULL

  params
}


param_check_restrictions <- function(x, profile, param) {
  subparams <- c(
    "length", "width", "height", "axleload", "weight", "hazmat",
    "surface_type", "track_type", "smoothness_type", "maximum_sloped_kerb",
    "maximum_incline", "maximum_width"
  )
  param_check_named(x, subparams, param)

  for (p in names(x)) {
    val <- x[[p]]
    param_check_profile(profile, param)
    switch(
      p,
      length = param_check_number(val, min = 0, param = p),
      width = param_check_number(val, min = 0, param = p),
      height = param_check_number(val, min = 0, param = p),
      axleload = param_check_number(val, min = 0, param = p),
      weight = param_check_number(val, min = 0, param = p),
      hazmat = param_check_flag(val, param = p),
      surface_type = param_check_string(val, param = p),
      track_type = param_check_match(val, param = p),
      smoothness_type = param_check_match(val, param = p),
      maximum_sloped_kerb = param_check_number(val, min = 0, param = p),
      maximum_incline = param_check_number(
        val,
        whole = TRUE,
        min = 0,
        max = 100,
        param = p
      ),
      minimum_width = param_check_number(val, min = 0, param = p),
      consider_unknown_parameter(p)
    )
  }
}


param_check_weightings <- function(x, profile, param) {
  subparams <- c("green", "quiet", "shadow", "steepness_difficulty")
  param_check_named(x, subparams, param)

  for (p in names(x)) {
    val <- x[[p]]
    param_check_profile(profile, param)
    switch(
      p,
      green = param_check_number(val, min = 0, max = 1, param = p),
      quiet = param_check_number(val, min = 0, max = 1, param = p),
      shadow = param_check_number(val, min = 0, max = 1, param = p),
      steepness_difficulty = param_check_match(val, param = p),
      consider_unknown_parameter(p)
    )
  }
}


param_check_round_trip <- function(x, profile, param) {
  subparams <- c("length", "points", "seed")
  param_check_named(x, subparams, param)

  for (p in names(x)) {
    val <- x[[p]]
    param_check_profile(profile, param)
    switch(
      p,
      length = param_check_number(val, param = p),
      points = param_check_number(val, whole = TRUE, min = 1, param = p),
      seed = param_check_number(val, whole = TRUE, min = 1),
      consider_unknown_parameter(p)
    )
  }

}


param_check_alternative_routes <- function(x, profile, param) {
  subparams <- c("share_factor", "target_count", "weight_factor")
  param_check_named(x, subparams, param)

  for (p in names(x)) {
    val <- x[[p]]
    param_check_profile(profile, param)
    switch(
      p,
      share_factor = param_check_number(val, min = 0, max = 1, param = p),
      target_count = param_check_number(val, whole = TRUE, min = 1, param = p),
      weight_factor = param_check_number(val, min = 0, param = p),
      consider_unknown_parameter(p)
    )
  }
}


param_check_bearings <- function(x, n, param) {
  x_fmt <- as.matrix(x)
  cond <- identical(nrow(x), as.integer(n)) && isTRUE(ncol(x) %in% c(1, 2))
  if (cond) {
    cli::cli_abort(c(
      "Parameter {.field {param}} must be a two-column matrix or a numeric vector.",
      "i" = "Got {x} instead."
    ))
  }

  x_fmt <- stats::na.omit(x_fmt)
  if (any(x_fmt > 360 | x_fmt < 0)) {
    abort(paste(
      "Parameter {.field {param}} must consist only of values",
      "between 0 and 360 (or NA)."
    ))
  }
}


param_check_radiuses <- function(x, n, param) {
  param_check_number(x, multiple = TRUE, whole = TRUE, min = -1, param = param)

  cond <- any(length(x) == c(1, n))
  if (!cond) {
    cli::cli_abort(c(
      paste(
        "Parameter {.field {param}} must be either contain a single value or as",
        "many values as there are segments."
      ),
      "i" = "Got {length(x)} values and {n} segments instead."
    ))
  }
}


param_check_poly <- function(x, param) {
  cond <- is_sf(x) && all(sf::st_is(x, c("POLYGON", "MULTIPOLYGON")))
  if (!cond) {
    cli::cli_abort(paste(
      "Parameter {.field {param}} must be an sf object consisting of only",
      "POLYGONs or MULTIPOLYGONs."
    ))
  }
}


param_check_profile <- function(profile, param) {
  required <- switch(
    param,
    allow_unsuitable = "wheelchair",
    avoid_borders = "driving",
    avoid_countries = "driving",
    axleload = "driving-hgv",
    bearings = "cycling",
    green = "foot",
    hazmat = "driving-hgv",
    height = "driving-hgv",
    length = "driving-hgv",
    maximum_incline = "wheelchair",
    maximum_sloped_kerb = "wheelchair",
    minimum_width = "wheelchair",
    quiet = "foot",
    shadow = "foot",
    smoothness_type = "wheelchair",
    surface_quality_known = "wheelchair",
    surface_type = "wheelchair",
    track_type = "wheelchair",
    vehicle_type = "driving-hgv",
    weight = "driving-hgv",
    width = "driving-hgv",
    profile
  )

  if (!startsWith(profile, required)) {
    cli::cli_abort(
      "Parameter {.field {param}} requires routing profile of type {.val {required}}.",
      "i" = "Got profile {.val {profile}} instead."
    )
  }
}


param_check_string <- function(x, param) {
  param_check_length(x, length = 1, param)
  param_check_vector(x, ptype = "character", param = param)
}


param_check_true_or_match <- function(x, param) {
  if (!isTRUE(x)) {
    param_check_match(x, param = param)
  }
}


param_check_flag <- function(x, param) {
  param_check_length(x, length = 1, param)
  param_check_vector(x, ptype = "logical", param = param)
}


param_check_vector <- function(x, ptype, param) {
  cond <- typeof(x) %in% ptype && !any(is.na(x))
  if (!cond) {
    cli::cli_abort(
      "Parameter {.field {param}} must be of type {ptype}.",
      "i" = "Got {typeof(x)} instead."
    )
  }
}


param_check_length <- function(x, length, param) {
  cond <- length(x) == length
  if (!cond) {
    cli::cli_abort(
      "Parameter {.field {param}} must be of length {length}, not {length(x)}"
    )
  }
}


param_check_match <- function(x, match = NULL, multiple = TRUE, param) {
  match <- match %||% param_lists[[param]]
  param_check_vector(x, "character", param)

  if (!multiple) {
    param_check_length(x, length = 1, param = param)
  }

  cond <- x %in% match
  if (!all(cond)) {
    faulty <- names(x)[!cond]
    cli::cli_abort(c(
      "Parameter {.field {param}} must contain any of the following values: {.val {match}}.",
      "i" = "{.val {faulty}} {?is/are} not {?a/} valid value{?s}."
    ))
  }
}


param_check_number <- function(x,
                               whole = FALSE,
                               multiple = FALSE,
                               min = -Inf,
                               max = Inf,
                               param) {
  cond <- is_number(x, multiple = multiple)
  if (whole) {
    cond <- cond && is_integerish(x)
  }

  if (!cond) {
    fmt <- ifelse(whole, "whole number", "single number")
    cli::cli_abort(sprintf("Parameter {.field {.field {param}}} must be a %s.", fmt))
  }

  cond <- all(x >= min)
  if (!cond) {
    cli::cli_abort("Parameter {.field {param}} must be greater than or equal to {min}.")
  }

  cond <- all(x <= max)
  if (!cond) {
    cli::cli_abort("Parameter {.field {param}} must be less than or equal to {max}.")
  }
}


param_check_named <- function(x, names, param) {
  cond <- names(x) %in% names
  if (!all(cond)) {
    faulty <- names(x)[!cond]
    cli::cli_abort(c(
      "Parameter {.field {param}} can contain names {.val {names}}",
      "i" = "{.val {faulty}} {?is/are} not {?a/} valid parameter{?s} for {.field {param}}."
    ))
  }
}


validate_param_names <- function(params, endpoint) {
  params_dup <- duplicated(names(params))
  if (any(params_dup)) {
    abort(
      "Duplicated ORS option{?s} {.var {names(params[params_dup])}}",
      class = "param_duplicated_error"
    )
  }

  supported_params <- switch(
    endpoint %||% "",
    inspect = c(
      "bearings", "alternative_routes", "geometry_simplify",
      "continue_straight", "preference", "radiuses", "maximum_speed",
      "attributes", "extra_info", "elevation", "skip_segments",
      "roundabout_exits", "maneuvers", "suppress_warnings", "id",
      "instructions", "instructions_format", "language",
      "avoid_borders", "avoid_countries", "avoid_features", "avoid_polygons",
      "round_trip", "vehicle_type", "restrictions", "weightings",
      "allow_unsuitable", "surface_quality_known"
    ),
    pairwise = c(
      "bearings", "geometry_simplify", "continue_straight",
      "preference", "radiuses", "maximum_speed", "attributes", "extra_info",
      "elevation", "skip_segments", "suppress_warnings",
      "avoid_borders", "avoid_countries", "avoid_features", "avoid_polygons",
      "vehicle_type", "restrictions", "weightings", "allow_unsuitable",
      "surface_quality_known"
    ),
    accessibility = c(
      "id", "avoid_borders", "avoid_countries", "avoid_features",
      "avoid_polygons", "vehicle_type", "restrictions", "weightings",
      "allow_unsuitable", "surface_quality_known"
    ),
    return()
  )

  params_compatible <- names(params) %in% supported_params
  if (!all(params_compatible)) {
    abort(
      paste(
        "The following parameters are incompatible with `ors_{endpoint}`:",
        "{.val {names(params)[params_compatible]}}"
      ),
      class = "param_incompatible_error"
    )
  }
}


consider_unknown_parameter <- function(param) {
  if (isFALSE(getOption("rors_allow_unknown_params", FALSE))) {
    abort(c(
      "Unknown ORS parameter {.var {.field {param}}}.",
      "i" = paste(
        "If you need to pass unknown parameters, you can set",
        "{.code options(rors_allow_unknown_params = TRUE)}."
      )
    ), class = "param_unknown_error")
  }
}


param_lists <- list(
  avoid_borders = c("all", "controlled", "none"),
  avoid_features = c("highways", "tollways", "ferries"),
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
  steepness_difficulty = 0:3,
  instructions_format = c("text", "html"),
  language = c(
    "de", "de-de", "en", "en-us", "eo", "eo-eo", "es", "es-es", "fr", "fr-fr",
    "gr", "gr-gr", "he", "he-il", "hu", "hu-hu", "id", "id-id", "it", "it-it",
    "ja", "ja-jp", "ne", "ne-np", "nl", "nl-nl", "nb", "nb-no", "pl", "pl-pl",
    "pt", "pt-pt", "ro", "ro-ro", "ru", "ru-ru", "tr", "tr-tr", "zh", "zh-cn",
    "cs", "cs-cz"
  )
)
