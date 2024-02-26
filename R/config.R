detect_config <- function(dir) {
  configs <- c(
    conf_dir_json = file.path(dir, "docker", "conf", "ors-config.json"),
    etc_dir = "/etc/openrouteservice/ors-config.yml",
    home_dir = "~/.openrouteservice/ors-config.yml",
    runtime_dir = file.path(dir, "docker", "ors-config.yml"),
    conf_dir = file.path(dir, "docker", "conf", "ors-config.yml")
  )

  config_found <- vapply(configs, file.exists, FUN.VALUE = logical(1))
  configs <- configs[config_found]

  config_path <- utils::tail(configs, 1)

  if (identical(names(config_path), "conf_dir_json")) {
    link <- cli::style_hyperlink(
      text = "Running with Docker",
      url = paste0(
        "https://giscience.github.io/openrouteservice/run-instance/",
        "installation/running-with-docker#customization"
      )
    )
    cli::cli_warn(c(
      "!" = "A JSON-style configuration file was mounted.",
      "i" = paste(
        "JSON configs are deprecated and not supported by {.pkg rors}.",
        "See {link} for details."
      )
    ))
  }

  if (length(config_path)) config_path
}


write_config <- function(config, file = NULL) {
  handlers <- list(
    logical = function(x) {
      structure(ifelse(x, "true", "false"), class = "verbatim")
    },
    numeric = function(x) {
      intx <- as.integer(x)
      if (all.equal(x, intx))
        x <- as.integer(x)
      x
    },
    "NULL" = function(x) {
      structure("", class = "verbatim")
    }
  )

  config <- yaml::as.yaml(
    config,
    precision = 22,
    handlers = handlers,
    indent.mapping.sequence = TRUE
  )

  if (!is.null(file)) {
    cat(config, file = file, sep = "")
  } else {
    config
  }
}


compare_config <- function(x, y) {
  if (is.null(x)) return(names(y))
  old_ep <- names(x)
  new_ep <- names(y)
  is_added <- !new_ep %in% old_ep
  ep_added <- new_ep[is_added]

  # if not all endpoints are brand-new, look for differences
  if (!all(is_added)) {
    y_changed <- y[!is_added]
    ep_changed <- names(y_changed)[!vapply(
      names(y_changed),
      function(i) equivalent_list(x[[i]], y[[i]]),
      logical(1)
    )]
    new_ep <- c(ep_added, ep_changed)
  }

  new_ep
}


compare_endpoints <- function(x, y) {
  if (!any(names(y) %in% endpoints())) {
    cli::cli_abort(c(
      "{.var ...} must contain a valid endpoint as argument name.",
      "i" = "Valid endpoints include: {.val {all_endpoints}}"
    ))
  }

  compare_config(x, y)
}


change_endpoints <- function(self, ...) {
  valid_ep <- c("routing", "matrix", "isochrone", "snap")
  config <- self$config$parsed
  dots <- list(...)

  dots <- dots[names(dots) %in% endpoints()]
  dots <- dots[lengths(dots) > 0]
  names(dots)["snap" %in% names(dots)] <- "Snap"

  if (is.null(config$ors$endpoints)) {
    config$ors$endpoints <- list()
  }

  for (endpoint in names(dots)) {
    if (is.null(config$ors$endpoints[[endpoint]])) {
      config$ors$endpoints[[endpoint]] <- dots[[endpoint]]
    } else {
      config$ors$endpoints[[endpoint]] <- modify_list(
        x = config$ors$endpoints[[endpoint]],
        y = dots[[endpoint]]
      )
    }
  }

  config$ors$endpoints
}


insert_profiles <- function(self, private, ...) {
  dots <- list(...)
  engine <- self$config$parsed$ors$engine

  corrupt <- logical(length(dots))
  for (i in seq_along(dots)) {
    prof <- dots[[i]]

    corrupt[i] <- FALSE
    if (!inherits(prof, "ors_profile")) {
      if (is.character(prof)) {
        prof <- ors_profile(prof, template = TRUE)
      } else {
        corrupt[i] <- TRUE
      }
    }

    name <- names(prof)
    if (identical(name, "profile_default")) {
      engine[[name]] <- prof[[name]]
    } else {
      engine$profiles[[name]] <- prof[[name]]
    }
  }

  if (any(corrupt)) {
    corrupt <- which(corrupt)
    ors_cli(warn = list(c("!" = paste(
      "{cli::qty(length(corrupt))} Arguments {corrupt} cannot be coerced to",
      "class {.cls ors_profile} and will be skipped."
    ))))
  }

  engine
}


remove_profiles <- function(self, ...) {
  dots <- c(...)
  engine <- self$config$parsed$ors$engine

  # handle profiles and default differently
  rm_default <- "default" %in% dots
  dots <- setdiff(dots, "default")

  # remove profiles
  pnames <- vapply(engine$profiles, "[[", "profile", FUN.VALUE = character(1))
  del <- pnames %in% dots | names(pnames) %in% dots
  engine$profiles[del] <- NULL

  # remove default if needed
  if (rm_default) {
    engine$profile_default <- NULL
  }

  engine
}


get_profile_names <- function(profiles) {
  nm <- vapply(seq_along(profiles), FUN.VALUE = character(1), function(i) {
    x <- profiles[[i]]
    if (inherits(x, "ors_profile")) {
      x[[1]]$profile
    } else if (is.list(x)) {
      bprof <- base_profiles()
      if ("profile" %in% names(x)) {
        x$profile
      } else if (names(profiles[i]) %in% names(bprof)) {
        bprof[[names(profiles[i])]]
      } else {
        NA_character_
      }
    } else if (is.character(x)) {
      x
    } else {
      NA_character_
    }
  })

  nm <- nm[!is.na(nm)]
  names(nm) <- names(profiles)
  nm
}


get_all_profiles <- function() {
  c(
    "car", "hgv", "bike-regular", "bike-mountain", "bike-road", "bike-electric",
    "walking", "hiking", "wheelchair", "public-transport"
  )
}


endpoints <- function() {
  c("routing", "matrix", "isochrone", "snap")
}

base_profiles <- function() {
  list(
    "car" = "driving-car",
    "hgv" = "driving-hgv",
    "bike-regular" = "cycling-regular",
    "bike-mountain" = "cycling-mountain",
    "bike-electric" = "cycling-electric",
    "bike-road" = "cycling-road",
    "walking" = "foot-walking",
    "hiking" = "foot-hiking",
    "wheelchair" = "wheelchair",
    "public-transport" = "public-transport"
  )
}


is_base_profile <- function(profile) {
  bprof <- base_profiles()
  profile %in% bprof || profile %in% names(bprof)
}


#' ORS profile
#'
#' @description
#' Construct the configuration for an ORS profile. The \code{ors_profile}
#' object created by this function can be used as input to the
#' \code{$add_profiles()} method of \code{\link[=ORSLocal]{ORS instances}}.
#'
#' @param name \code{[character]}
#'
#' Name of the ORS profile. If a length-2 vector is passed, the first
#' value is taken as the actual name and the second value is taken as the
#' title of the profile in the configuration file
#' (e.g. \code{c("driving-car", "car")}).
#'
#' @param ...
#'
#' Configuration parameters for the ORS profile. Must be key-value pairs. For
#' details, refer to the \href{https://giscience.github.io/openrouteservice/run-instance/configuration/ors/engine/profiles}{configuration reference}.
#'
#' @param template \code{logical}
#'
#' Whether to use a template if \code{name} is one of the base profiles
#' (see details).
#'
#' @returns An object of class \code{ors_profile} that can be used to add
#' profiles to \code{\link{ors_instance}}
#'
#' @details
#' ORS defines a number of base profiles including:
#'
#' \itemize{
#'  \item{driving-car}
#'  \item{driving-hgv}
#'  \item{cycling-regular}
#'  \item{cycling-road}
#'  \item{cycling-electric}
#'  \item{cycling-mountain}
#'  \item{foot-walking}
#'  \item{foot-hiking}
#'  \item{wheelchair}
#'  \item{public-transport}
#' }
#'
#' All base profiles have a pre-defined template. Profiles other than
#' the base profiles can also be constructed with \code{ors_profile}, but
#' need some more sophisticated preparation (see e.g. Butzer 2017).
#'
#' @references Butzer, A.S. (2017). Erstellung eines Routing-Profils für
#' Feuerwehrfahrzeuge auf Basis von OpenStreetMap-Daten. Bachelor Thesis.
#' Ruprecht Karl University of Heidelberg.
#'
#'
#' @examples
#' # Create a car profile with pre-defined defaults
#' ors_profile("driving-car")
#'
#' # Defaults can be modified
#' ors_profile("driving-car", elevation = FALSE)
#'
#' # Create a walking profile with given encoder options
#' ors_profile("bike-regular", template = FALSE, encoder_options = list(turn_costs = FALSE))
#'
#' @export
ors_profile <- function(name = NULL, ..., template = TRUE) {
  assert_that(length(name) <= 2)
  bprof <- base_profiles()

  if (is.null(name)) {
    title <- "profile_default"
    name <- NULL
  } else if (length(name) == 2) {
    title <- name[2]
    name <- name[1]
  } else if (is_base_profile(name)) {
    if (name %in% bprof) {
      title <- stats::setNames(names(bprof), bprof)[name]
    } else {
      title <- name
      name <- bprof[[name]]
    }
  } else {
    title <- name
  }

  if (isFALSE(template)) {
    args <- list(...)

    if (!"enabled" %in% names(args)) {
      args <- c(enabled = TRUE, args)
    }

    if (!is.null(name)) {
      args <- c(profile = name, args)
    }

    profile <- list(args)
    names(profile) <- title
    structure(profile, class = "ors_profile")
  } else {
    profile <- make_default_profile(name)
    dots <- list(...)
    for (opt in names(dots)) profile[[1]][[opt]] <- dots[[opt]]
    profile
  }
}


make_default_profile <- function(profile) {
  default <- ors_profile(
    template = FALSE,
    elevation = TRUE,
    elevation_smoothing = TRUE,
    encoder_flags_size = 4
  )

  if (is.null(profile)) return(default)

  switch(
    profile,
    "default" = default,
    "driving-car" = ors_profile(
      "driving-car",
      template = FALSE,
      encoder_options = list(
        turn_costs = TRUE,
        block_fords = FALSE,
        use_acceleration = TRUE,
        maximum_grade_level = 1,
        conditional_access = TRUE,
        conditional_speed = TRUE
      ),
      elevation = TRUE,
      preparation = list(
        min_network_size = 200,
        min_one_way_network_size = 200,
        methods = list(
          ch = list(
            enabled = TRUE,
            threads = 1,
            weightings = "fastest"
          ),
          lm = list(
            enabled = FALSE,
            threads = 1,
            weightings = "fastest",
            landmarks = 16
          ),
          core = list(
            enabled = TRUE,
            threads = 1,
            weightings = "fastest,shortest",
            landmarks = 64
          )
        )
      ),
      execution = list(
        methods = list(
          lm = list(active_landmarks = 8),
          core = list(disabling_allowed = TRUE, active_landmarks = 6)
        )
      )
    ),
    "driving-hgv" = ors_profile(
      "driving-hgv",
      template = FALSE,
      maximum_speed_lower_bound = 75L,
      elevation = TRUE,
      encoder_options = list(
        turn_costs = TRUE,
        block_fords = FALSE,
        use_acceleration = TRUE),
      preparation = list(
        min_network_size = 200L,
        min_one_way_network_size = 200L,
        methods = list(
          ch = list(
            enabled = TRUE,
            threads = 1L,
            weightings = "recommended"
          ),
          lm = list(
            enabled = TRUE,
            threads = 1L,
            weightings = "recommended",
            landmarks = 16L
          ),
          core = list(
            enabled = TRUE,
            threads = 1L,
            weightings = "recommended,shortest",
            landmarks = 32L
          ),
          fastisochrones = list(
            enabled = TRUE,
            threads = 12L,
            weightings = "recommended, shortest",
            maxcellnodes = 5000L
          )
        )
      ),
      execution = list(
        methods = list(
          lm = list(active_landmarks = 8L),
          core = list(disabling_allowed = TRUE, active_landmarks = 6L)
        )
      )
    ),
    "cycling-regular" = ors_profile(
      "cycling-regular",
      template = FALSE,
      encoder_options = list(
        consider_elevation = FALSE,
        turn_costs = TRUE,
        block_fords = FALSE
      ),
      preparation = list(
        min_network_size = 200L,
        min_one_way_network_size = 200L,
        methods = list(
          core = list(
            enabled = TRUE,
            threads = 1L,
            weightings = "recommended,shortest",
            landmarks = 32L
          )
        ),
        elevation = TRUE
      ),
      execution = list(methods = list(core = list(
        disabling_allowed = TRUE,
        active_landmarks = 6L
      )))
    ),
    "cycling-mountain" = ors_profile(
      "cycling-mountain",
      template = FALSE,
      maximum_snapping_radius = 10L,
      encoder_options = list(consider_elevation = FALSE, turn_costs = TRUE),
      elevation = TRUE
    ),
    "cycling-road" = ors_profile(
      "cycling-road",
      template = FALSE,
      encoder_options = list(
        consider_elevation = FALSE,
        turn_costs = FALSE,
        block_fords = FALSE
      ),
      elevation = TRUE
    ),
    "cycling-electric" = ors_profile(
      "cycling-electric",
      template = FALSE,
      encoder_options = list(
        consider_elevation = FALSE,
        turn_costs = TRUE,
        block_fords = FALSE
      ),
      elevation = TRUE
    ),
    "foot-walking" = ors_profile(
      "foot-walking",
      template = FALSE,
      interpolate_bridges_and_tunnels = FALSE,
      elevation = TRUE,
      encoder_options = list(block_fords = FALSE)
    ),
    "foot-hiking" = ors_profile(
      "foot-hiking",
      template = FALSE,
      elevation = TRUE,
      encoder_options = list(block_fords = FALSE)
    ),
    "wheelchair" = ors_profile(
      "wheelchair",
      template = FALSE,
      maximum_snapping_radius = 50,
      elevation = TRUE,
      encoder_options = list(block_fords = FALSE)
    ),
    "public-transport" = ors_profile(
      "public-transport",
      template = FALSE,
      maximum_visited_nodes = 1000000,
      elevation = TRUE,
      encoder_options = list(block_fords = FALSE)
    ),
    cli::cli_abort("No template defined for profile {.val profile}.")
  )
}
