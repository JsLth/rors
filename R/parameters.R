#' Construct ORS parameters
#'
#' @description
#' Create a list of parameters that can be passed to a function that supports
#' additional parameters. For further information on parameters, visit the
#' \href{https://openrouteservice.org/dev/#/api-docs/v2}{API playground} or
#' the \href{https://giscience.github.io/openrouteservice/api-reference/endpoints/directions/routing-options}{backend reference}.
#' \code{\link{param_info}} provides an overview of the checks and
#' preparations performed by \code{ors_params}.
#'
#' Note that this function provides a means to manually construct parameters.
#' For all high-level endpoint functions such as \code{\link{ors_inspect}},
#' parameters can be passed as dot arguments.
#'
#' @param profile \code{[character]}
#'
#' A routing profile defined by ORS. Not all combinations of
#' profiles and parameters are supported.
#' @param n \code{[integer]}
#'
#' Number of observations used for routing. This value refers to
#' segments as described in \code{\link{ors_inspect}}. For use in
#' \code{\link{ors_pairwise}} it should always be 1.
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
#' @param alternative_routes \code{{list}}
#'
#' List of options for generating alternative routes. Can be one of the
#' following options:
#'
#' \describe{
#'  \item{\code{target_count}}{Number of alternatives that should be computed.}
#'  \item{\code{weight_factor}}{Multiplier that specifies by what factor an
#'  alternative routes is allowed to diverge from the optimal route.
#'  Defaults to 1.4.}
#'  \item{\code{share_factor}}{Multiplier that specificies the fraction an
#'  alternative route is allowed to share with the optimal route. Defaults
#'  to 0.6.}
#' }
#'
#' Because this parameter changes the output format, it is only available
#' in \code{ors_inspect}.
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
#' Vector of route segments to skip. Must be values between 1 and n where
#' 1 represents the segment between the first and second input point.
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
#' or by running \code{\link{country_info}}.
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
#' @param round_trip \code{[list]}
#'
#' List of options for generating round trips. Can include the following
#' options:
#'
#' \describe{
#'  \item{\code{length}}{Target length of the round trip (in m)}
#'  \item{\code{points}}{Number of points to create the round trip}
#'  \item{\code{seed}}{RNG seed to control randomness in the direction of the
#'  round trip}
#' }
#'
#' Because this parameter changes the output format, it is only available
#' in \code{ors_inspect}.
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
#'  \item{\code{hazmat}}{Whether to adjust routing for transportation hazardous goods,
#'  i.e. avoid protected areas, for \code{"driving-hgv"}. Defaults to
#'  \code{FALSE}.}
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
#' @export
ors_params <- function(profile,
                       n = NULL,
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
                       allow_unsuitable = FALSE) {
  params <- as.list(match.call())[-1]
  params[c("n", "profile")] <- NULL
  prepare_ors_params(params, profile, n)
}



#' Formats ORS options, checks if they're valid and constructs a list that
#' can be used to create an http query
#' @noRd
prepare_ors_params <- function(params, profile, n = NULL) {
  params %||% return()
  params <- format_ors_params(params, profile, n)
  check_ors_params(params)
  construct_ors_params(params)
}


format_ors_params <- function(params, profile, n) {
  validate_param_names(params)
  info <- param_info()
  pnames <- names(params)

  params <- lapply(
    pnames,
    params = params,
    info = info,
    function(name, params, info) {
      val <- params[[name]]

      if (inherits(val, "list")) {
        info <- info[info$parent %in% name, ]
        param <- list(Recall(names(val), val, info))
        names(param) <- names(val)
        param
      } else {
        parent <- unique(info$parent)
        info <- info[info$name %in% name, ]

        if (!nrow(info)) {
          cli::cli_abort(
            paste(
              "Parameter {.val {name}} does not",
              "belong to {.val {unique(parent)}}."
            ),
            class = "param_invalid_child_error"
          )
        }

        args <- list(x = val, profile = profile, n = n)
        do.call(param_prepare, c(args, as.list(info)))
      }
    }
  )

  names(params) <- pnames
  params
}


#' Format and check ORS vector parameters
#' @noRd
param_prepare <- function(x,
                          name,
                          n,
                          profile,
                          type,
                          box,
                          scalar,
                          lim,
                          match,
                          allowed,
                          ...) {
  checks <- c("type", "profile", "length", "lim", "match", "poly")
  names(checks) <- checks

  checks <- vswitch(
    checks,
    FUN.VALUE = logical(1),
    USE.NAMES = TRUE,
    type = param_check_type(x, type),
    profile = param_check_profile(profile, allowed),
    length = param_check_length(x, name, scalar, n),
    lim = param_check_lim(x, name, lim, n),
    match = param_check_match(x, name, match, profile, allowed),
    poly = param_check_poly(x, name)
  )

  if (all(checks)) {
    x <- param_format_poly(x, name)
    x <- param_format_matrix(x, name)
    x <- param_format_boxed(x, box)
  }

  attr(x, "check") <- param_verify(checks, name)
  x
}


param_check_type <- function(x, type) {
  switch(
    type,
    logical = all(is_true_or_false(x, flag = FALSE)),
    double = is.double(x),
    integer = is_integerish(x),
    character = is.character(x),
    TRUE
  )
}


param_check_profile <- function(profile, allowed) {
  startsWith(profile, allowed %NA% profile)
}


param_check_length <- function(x, name, scalar, n) {
  if (identical(name, "radiuses")) {
    any(length(x) == c(1, n))
  } else if (identical(name, "bearings")) {
    x <- as.matrix(x)
    identical(nrow(x), as.integer(n)) && isTRUE(ncol(x) %in% c(1, 2))
  } else {
    cmp_fun <- ifelse(isTRUE(scalar), `==`, `>=`)
    cmp_fun(length(x), 1)
  }
}


param_check_lim <- function(x, name, lim, n) {
  if (!lim) return(TRUE)
  param_match_lim(name, x, n)
}


param_check_match <- function(x, name, match, profile, allowed) {
  if (!match) return(TRUE)

  if (identical(name, "extra_info")) {
    allowed <- startsWith(profile, param_extra_info_allowed(x))
    x[!allowed] <- NA
  }

  identical(x, param_match_arg(name, x))
}


param_check_poly <- function(x, name) {
  if (identical(name, "avoid_polygons")) {
    is_sf(x) && all(sf::st_is(x, c("POLYGON", "MULTIPOLYGON")))
  } else {
    TRUE
  }
}


param_format_boxed <- function(x, box) {
  if (box) list(x)
  x
}


param_format_poly <- function(x, name) {
  if (identical(name, "avoid_polygons")) {
    tempf <- tempfile(fileext = ".geojson")
    on.exit(unlink(tempf))
    capture.output(
      sf::st_write(sf::st_geometry(x), tempf),
      type = "output"
    )
    x <- jsonlite::read_json(tempf)
    x$name <- NULL
    class(x) <- "ors_geojson"
    x
  }
  x
}


param_format_matrix <- function(x, name) {
  if (identical(name, "bearings")) {
    x <- unname(apply(
      as.matrix(x),
      MARGIN = 1,
      function(x) drop_na(unname(c(x))),
      simplify = FALSE
    ))
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
      length =  "invalid length",
      lim = "exceeds defined value limits",
      match = paste0(
        "undefined values",
        if (identical(name, "extra_info")) " (possibly wrong profile)"
      ),
      poly = "invalid sf object"
    )
  }
}


validate_param_names <- function(params) {
  params_ok <- names(params) %in% param_info()$name
  if (!all(params_ok)) {
    cli::cli_abort(
      "Unknown ORS option{?s} {.var {names(params[!params_ok])}}.",
      class = "param_unknown_error"
    )
  }

  params_dup <- duplicated(names(params))
  if (any(params_dup)) {
    cli::cli_abort(
      "Duplicated ORS option{?s} {.var {names(params[params_dup])}}",
      class = "param_duplicated_error"
    )
  }
}


check_ors_params <- function(params) {
  checks <- drop_null(lapply(flatten_list(params), attr, "check"))

  if (length(checks)) {
    reasons <- vapply(checks, paste, character(1), collapse = ", ")
    names(checks) <- gsub(
      ".",
      sprintf(" %s ", cli::symbol$arrow_right),
      names(checks),
      fixed = TRUE
    )
    bad_params <- paste(
      cli::col_yellow(cli::symbol$bullet),
      sprintf("{cli::col_yellow('%s')}:", names(checks)),
      reasons
    )
    n_bad <- length(bad_params)
    names(bad_params) <- rep(" ", n_bad)

    cli::cli_abort(
      c(
        paste(
          "The following {cli::qty(n_bad)} parameter{?s}",
          "{?is/are} invalid and need{?s/} to be revised:"
        ),
        bad_params
      ),
      class = "param_invalid_error"
    )
  }
}


construct_ors_object <- function(obj, name, params, info) {
  parent <- info$parent[info$name %in% name]
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

  class(params) <- "ors_params"
  params
}


#' Specifies which routing profile is allowed for a given extra_info value.
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


#' Parameter info
#' @description
#' Get information on the checks and preparations performed by
#' \code{\link{ors_params}} for additional ORS parameters.
#'
#' @returns A dataframe or tibble with the following columns:
#' \describe{
#'  \item{\code{name}}{Name of the parameter}
#'  \item{\code{parent}}{Parent object of the parameter if it is nested, NA otherwise}
#'  \item{\code{match}}{Does the parameter have a predefined list of values?}
#'  \item{\code{scalar}}{Should the parameter be length-1?}
#'  \item{\code{type}}{R type of the parameter value}
#'  \item{\code{lim}}{Is parameter subject to numerical limits?}
#'  \item{\code{allowed}}{Which profiles are required to use the parameter?}
#'  \item{\code{poly}}{Should the parameter be formatted as a polygon?}
#'  \item{\code{box}}{Does the parameter need boxing before POSTing?}
#'  \item{\code{inspect}}{Is the parameter available for ors_inspect?}
#'  \item{\code{pariwise}}{Is the parameter available for ors_pairwise?}
#'  \item{\code{accessibility}}{Is the parameter available for ors_accessibility?}
#' }
#'
#' @export
param_info <- function() {
  # constructive::construct(dplyr::arrange(
  #   param_info(),
  #   match(parent, c(
  #     NA, "options", "profile_params", "restrictions", "weightings",
  #     "round_trip", "alternative_routes"
  #   ))
  # ), constructive::opts_atomic(compress = FALSE))
  tibble::tibble(
    name = c(
      "options", "bearings", "alternative_routes", "geometry_simplify",
      "continue_straight", "preference", "radiuses", "maximum_speed", "attributes",
      "extra_info", "elevation", "skip_segments", "roundabout_exits", "maneuvers",
      "suppress_warnings", "id", "instructions", "instructions_format", "language",
      "profile_params", "avoid_borders", "avoid_countries", "avoid_features",
      "avoid_polygons", "round_trip", "vehicle_type", "restrictions", "weightings",
      "allow_unsuitable", "surface_quality_known", "length", "width", "height",
      "axleload", "weight", "hazmat", "surface_type", "track_type",
      "smoothness_type", "maximum_sloped_kerb", "maximum_incline", "minimum_width",
      "steepness_difficulty", "green", "quiet", "shadow", "length", "points",
      "seed", "target_count", "weight_factor", "share_factor"
    ),
    parent = c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      "options", "options", "options", "options", "options", "options", "options",
      "profile_params", "profile_params", "profile_params", "profile_params",
      "restrictions", "restrictions", "restrictions", "restrictions",
      "restrictions", "restrictions", "restrictions", "restrictions",
      "restrictions", "restrictions", "restrictions", "restrictions", "weightings",
      "weightings", "weightings", "weightings", "round_trip", "round_trip",
      "round_trip", "alternative_routes", "alternative_routes",
      "alternative_routes"
    ),
    match = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
      TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    scalar = c(
      NA, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,
      FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, NA, TRUE, FALSE,
      FALSE, NA, NA, TRUE, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    ),
    type = c(
      NA, "double", NA, "logical", "logical", "character", "double", "double",
      "character", "character", "logical", "integer", "logical", "logical",
      "logical", "character", "logical", "character", "character", NA, "character",
      "double", "character", NA, NA, "character", NA, NA, "logical", "logical",
      "double", "double", "double", "double", "double", "logical", "character",
      "character", "character", "double", "integer", "double", "integer", "double",
      "double", "double", "double", "integer", "integer", "integer", "double",
      "double"
    ),
    lim = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,
      FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
      TRUE, FALSE, TRUE, TRUE, TRUE
    ),
    allowed = c(
      NA, "cycling", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      NA, NA, "driving", "driving", NA, NA, NA, "driving-hgv", NA, NA, "wheelchair",
      "wheelchair", "driving-hgv", "driving-hgv", "driving-hgv", "driving-hgv",
      "driving-hgv", "driving-hgv", "wheelchair", "wheelchair", "wheelchair",
      "wheelchair", "wheelchair", "wheelchair", "cycling", "foot", "foot", "foot",
      NA, NA, NA, NA, NA, NA
    ),
    box = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    inspect = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    ),
    pairwise = c(
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,
      TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE
    ),
    accessibility = c(
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
      FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE
    ),
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
    steepness_difficulty = 0:3,
    language_format = c("text", "html"),
    language = c(
      "de", "de-de", "en", "en-us", "eo", "eo-eo", "es", "es-es", "fr", "fr-fr",
      "gr", "gr-gr", "he", "he-il", "hu", "hu-hu", "id", "id-id", "it", "it-it",
      "ja", "ja-jp", "ne", "ne-np", "nl", "nl-nl", "nb", "nb-no", "pl", "pl-pl",
      "pt", "pt-pt", "ro", "ro-ro", "ru", "ru-ru", "tr", "tr-tr", "zh", "zh-cn",
      "cs", "cs-cz"
    )
  )

  table[match(values, table, nomatch = 0)]
}


param_match_lim <- function(name, value, n) {
  lims <- switch(
    name,
    radiuses = c(-1, Inf),
    maximum_speed = c(80, Inf),
    avoid_countries = c(1, 236),
    length = c(0, Inf),
    width = c(0, Inf),
    height = c(0, Inf),
    axleload = c(0, Inf),
    weight = c(0, Inf),
    maximum_sloped_kerb = c(0, Inf),
    maximum_incline = c(0, 100),
    minimum_width = c(0, Inf),
    green = c(0, 1),
    quiet = c(0, 1),
    shadow = c(0, 1),
    points = c(1, Inf),
    target_count = c(0, Inf),
    weight_factor = c(0, Inf),
    share_factor = c(0, Inf),
    skip_segments = c(1, n %||% 0),
    c(-Inf, Inf)
  )

  all(value >= lims[1]) && all(value <= lims[2])
}


#' Country code table
#' @description
#' Information about ORS countries and their associated country codes.
#' Useful for the \code{avoid_countries} parameter in \code{\link{ors_params}}.
#'
#' @returns A dataframe or tibble with the following columns:
#' \describe{
#'  \item{\code{country_id}}{Country codes used by ORS}
#'  \item{\code{name}}{English name of the country}
#' }
#'
#' @export
country_info <- function() {
  # rvest::read_html("https://giscience.github.io/openrouteservice/technical-details/country-list") |>
  #  rvest::html_table() |>
  #  purrr::pluck(1) |>
  #  dplyr::mutate(
  #    `name:en` = ifelse(!nzchar(`name:en`), NA, `name:en`),
  #    name = dplyr::coalesce(`name:en`, name)
  #  ) |>
  #  dplyr::select(-`name:en`) |>
  #  constructive::construct()
  data_frame(
    country_id = 1:236,
    name = c(
      "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Anguilla",
      "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria",
      "Azerbaijan", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium",
      "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina",
      "Botswana", "Brazil", "British Indian Ocean Territory",
      "British Sovereign Base Areas", "British Virgin Islands", "Brunei",
      "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada",
      "Cape Verde", "Cayman Islands", "Central African Republic", "Chad", "Chile",
      "China", "Colombia", "Comoros", "Congo-Brazzaville", "Congo-Kinshasa",
      "Cook Islands", "Costa Rica", "C\U{F4}te d'Ivoire", "Croatia", "Cuba",
      "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica",
      "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador",
      "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Falkland Islands",
      "Faroe Islands", "Federated States of Micronesia", "Fiji", "Finland",
      "France", "Gabon", "Gambia", "Georgia", "Germany", "Germany - Belgium",
      "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guatemala",
      "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras",
      "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland",
      "Isle of Man", "Israel", "Italy", "Jamaica", "Jangy-ayyl", "Japan", "Jersey",
      "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan",
      "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein",
      "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia",
      "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius",
      "Mexico", "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat",
      "Morocco", "Mozambique", "Myanmar", "name:en", "Namibia", "Nauru", "Nepal",
      "Netherlands - Belgium", "New Zealand", "Nicaragua", "Niger", "Nigeria",
      "Niue", "North Korea", "Norway", "Oman", "Pakistan", "Palau",
      "Palestinian Territories", "Panama", "Papua New Guinea", "Paraguay", "Peru",
      "Philippines", "Pitcairn Islands", "Poland", "Portugal", "Qatar", "Romania",
      "Russian Federation", "Rwanda", "Sahrawi Arab Democratic Republic",
      "Saint Helena - Ascension and Tristan da Cunha", "Saint Kitts and Nevis",
      "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino",
      "S\U{E3}o Tom\U{E9} and Pr\U{ED}ncipe", "Saudi Arabia", "Senegal", "Serbia",
      "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia",
      "Solomon Islands", "Somalia", "South Africa",
      "South Georgia and the South Sandwich Islands", "South Korea", "South Sudan",
      "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden",
      "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand",
      "The Bahamas", "The Netherlands", "Togo", "Tokelau", "Tonga",
      "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan",
      "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine",
      "United Arab Emirates", "United Kingdom", "United States of America",
      "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam",
      "Yemen", "Zambia", "Zimbabwe", "Border India - Bangladesh", "\U{CE}le Verte",
      "Border Azerbaijan - Armenia (Enclave AZE)", "Freezland Rock", "Border SI-HR",
      "Willis Island", "Chong-Kara",
      "\U{395}\U{3BB}\U{3BB}\U{3AC}\U{3B4}\U{3B1} - \U{3A0}\U{3B1}\U{3B3}\U{3B3}\U{3B1}\U{3AF}\U{3BF}",
      "Bristol Island", "Dist. Judges Court", "Border Kyrgyzstan - Uzbekistan",
      "Border Malawi - Mozambique", "\U{4E2D}\U{83EF}\U{6C11}\U{570B}"
    )
  )
}
