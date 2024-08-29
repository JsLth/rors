#' Local ORS instance
#' @description
#' This R6 class is the foundation for R6 classes \code{\link{ORSDocker}},
#' \code{\link{ORSJar}} and \code{\link{ORSWar}} and cannot be initialized.
#' It defines methods to manage and modify configurations and extracts of
#' OpenRouteService.
#'
#' @examples
#' \dontrun{
#' ors <- ors_instance(dir = "~", type = "docker")
#' ors <- ors_instance(dir = "~", type = "jar")
#' ors <- ors_instance(dir = "~", type = "war")
#' }
ORSLocal <- R6::R6Class(
  classname = "ORSLocal",
  inherit = ORSInstance,

  # Public ----
  public = list(
    #' @field paths List of relevant file paths for the ORS setup. Includes
    #' the top directory, compose file, config file and extract file.
    paths = list(),

    #' @field version Version of the local ORS backend
    version = NULL,

    #' @field config Information of the configuration file (\code{ors-config.yml}).
    #' The config file holds various options about the ORS instance. This
    #' field gives details about:
    #'
    #' \itemize{
    #'  \item{profiles: A named vector of active routing profiles}
    #'  \item{parsed: Parsed configuration file. When making changes to
    #'  this obhect, make sure to run \code{$update()} to apply the changes.
    #'  For details, refer to the
    #'  \href{https://giscience.github.io/openrouteservice/run-instance/configuration/}{ORS reference}.}
    #' }
    config = NULL,

    #' @field extract Information on the extract file. Contains the name and
    #' size of the selected extract file.
    extract = NULL,

    ## Meta ----
    #' @description
    #' Updates ORS instance. Use this to apply changes made either in the
    #' file system or to the \code{\link{ors_instance}} object itself. This
    #' method is automatically called when using any method of
    #' \code{ors_instance} that changes the ORS setup.
    #'
    #' @param what \code{[character]}
    #'
    #' Whether to change the file system with changes in R or update the
    #' instance object with changes in the file system. If \code{what = "self"},
    #' parses the relevant files in the ORS directory and updates the
    #' \code{ors_instance} object. If \code{what = "fs"}, updates the
    #' compose file and config file based on the changes made to the
    #' \code{ors_instance} object.
    update = function(what = c("fs", "self")) {
      what <- match.arg(what)
      switch(what, fs = private$.write(), self = private$.parse())
      private$.mount()
      invisible(self)
    },

    #' @description
    #' Prints a situation report of the ORS instance. Invokes all relevant
    #' print methods that summarize the current state of the instance
    #' object.
    report = function() {
      sitrep <- list(
        self,
        self$compose,
        self$extract,
        self$config
      )
      class(sitrep) <- "ors_local_sitrep"
      sitrep
    },


    ## Extract ----
    #' @description
    #' Download and set an OpenStreetMap extract for use in ORS. Wrapper for
    #' \code{\link[osmextract:oe_get]{oe_get()}}.
    #'
    #' @param place \code{[various]}
    #'
    #' Place name, \code{sf/sfc/bbox} object or bounding box for which to
    #' download an extract file. For details, refer to
    #' \code{\link[osmextract:oe_match]{oe_match()}}.
    #' @param provider \code{[character/NULL]}
    #'
    #' Extract provider to download extract from. Available
    #' providers can be found by running
    #' \code{\link[osmextract:oe_providers]{oe_providers()}}. If \code{NULL},
    #' tries all providers.
    #'
    #' @param timeout \code{[numeric]}
    #'
    #' Timeout for extract downloads. Defaults to \code{getOption("timeout")}.
    #' @param file \code{[character/NULL]}
    #'
    #' Path to a local OSM extract. Can either be a full path to any OSM file
    #' or the filename of an OSM file in the data folder of ORS. If file is
    #' specified, \code{place} and \code{provider} are ignored.
    #' @param do_use \code{[logical]}
    #'
    #' If \code{TRUE}, enables graph building with the new extract. If
    #' \code{FALSE}, does not change the compose file at all.
    #' @param ... Further arguments passed to
    #' \code{\link[osmextract:oe_get]{oe_get()}}.
    set_extract = function(place,
                           provider = "geofabrik",
                           timeout = NULL,
                           file = NULL,
                           do_use = TRUE,
                           ...) {
      if (is.null(file)) {
        file <- get_extract(
          self,
          private,
          place = place,
          provider = provider,
          timeout = timeout,
          ...
        )
      } else {
        file <- set_extract(self, private, file)
      }

      if (do_use && !is.null(file)) {
        ors_cli(info = list(c("*" = "Using extract {.val {basename(file)}}")))

        # set class attributes
        self$paths$extract <- file
        self$extract <- new_ors_extract(file)

        # set extract path in config
        docker <- if (private$.is_type("docker")) "files"
        fpath <- self$paths$top
        self$config$parsed$ors$engine$source_file <- relative_path(file, fpath)

        if (private$.is_type("docker")) {
          self$set_graphbuilding(TRUE)

          # ensure files volume is mounted
          self$compose$parsed <- modify_files_volume(
            self$compose$parsed,
            path = "files"
          )
        }
      }

      self$update()
      invisible(self)
    },

    #' @description
    #' Removes extract files from the data directory.
    #'
    #' @param ... File names of extract files in the data directory. All
    #' files that exist are removed. Can also be a single vector of file
    #' names.
    rm_extract = function(...) {
      docker <- if (private$.is_type("docker")) "files"
      old <- c(...)

      if (is.null(old)) {
        all_files <- list.files(file.path(self$paths$top, docker))
        old <- all_files[is_pbf(all_files)]
      }

      old <- file.path(self$paths$top, docker, old)
      changed <- vapply(old, file.exists, logical(1))

      if (any(changed)) {
        to_rm <- old[changed]
        to_rm_fmt <- basename(to_rm)
        cur_extract <- identify_extract(self$paths$top) %||% ""

        if (nzchar(cur_extract)) {
          to_rm_fmt[to_rm %in% cur_extract] <- paste(
            basename(cur_extract), cli::col_red("<- active extract")
          )
        }

        ors_cli(info = list(c("*" = "Removing extract files:")))
        ors_cli(bullets = list(stats::setNames(
          sprintf("- %s", to_rm_fmt), rep(" ", length(to_rm))
        )))

        yes_no(
          "Do you want to proceed?",
          no = cancel(),
          ask = private$.prompts
        )

        for (extract in to_rm) {
          unlink(extract, recursive = FALSE)
        }

        if (cur_extract %in% to_rm) {
          ors_cli(info = list(c(
            "*" = "Unsetting active extract file: {.emph {basename(cur_extract)}}"
          )))
          self$extract <- NULL
          self$config$parsed$ors$engine$source_file <- NULL
          self$compose$parsed <- modify_files_volume(self$compose$parsed, NULL)
          self$update()
        }
      }

      invisible(self)
    },


    ## Config ----
    #' @description
    #' Add routing profiles to the ORS configuration. ORS only builds routing
    #' graphs for active profiles.
    #'
    #' @param ... Objects of class \code{\link{ors_profile}} or character
    #' strings. If a character string is passed, it is interpreted as
    #' \code{ors_profile(..., template = TRUE)}.
    add_profiles = function(...) {
      old <- self$config$parsed$ors$engine
      new <- insert_profiles(self, private, ...)
      changed <- compare_config(old$profiles, new$profiles)
      changed_dflt <- "profile_default" %in% names(new) &&
        !equivalent_list(old$profile_default, new$profile_default)

      if (length(changed) || changed_dflt) {
        if (length(changed)) {
          ors_cli(info = list(c(
            "*" = "Adding {cli::qty(changed)} profile{?s}: {.field {changed}}"
          )))
        }

        if (changed_dflt) {
          ors_cli(info = list(c(
            "*" = "Adding profile defaults"
          )))
        }

        self$config$parsed$ors$engine <- new
        self$config$profiles <- names(new$profiles)
        self$update()
      }

      invisible(self)
    },

    #' @description
    #' Remove routing profiles from the ORS configuration.
    #'
    #' @param ... Names of routing profiles to remove. \code{"default"}
    #' removes profile defaults. Can also be a single character vector.
    rm_profiles = function(...) {
      old <- self$config$profiles
      new <- c(...)
      changed <- setdiff(new, "default") %in% c(old, names(old))
      changed_dflt <- "profile_default" %in% names(self$config$parsed$ors$engine) &&
        "default" %in% new

      if (any(changed) || changed_dflt) {
        if (any(changed)) {
          ors_cli(info = list(c("*" = paste(
            "Removing {cli::qty(length(new[changed]))}",
            "profile{?s}: {.field {new[changed]}}"
          ))))
        }

        if (changed_dflt) {
          ors_cli(info = list(c("*" = "Removing profile defaults")))
        }

        self$config$parsed$ors$engine <- remove_profiles(self, ...)
        self$config$profiles <- get_profile_names(
          self$config$parsed$ors$engine$profiles
        )
        self$update()
      }

      invisible(self)
    },

    #' @description
    #' Change endpoint-specific configurations. Specifies options that are
    #' relevant for entire API endpoints such as isochrones.
    #'
    #' @param ... \code{[list/NULL]}
    #'
    #' Named arguments containing the configuration for the endpoints.
    #' Available endpoits are \code{routing}, \code{isochrones},
    #' \code{matrix}, and \code{snap}. Refer to
    #' \href{https://github.com/GIScience/openrouteservice/blob/master/ors-api/src/main/resources/application.yml}{application.yml}
    #' for a list of defaults.
    set_endpoints = function(...) {
      old <- self$config$parsed$ors$endpoints
      new <- list(...)
      changed <- compare_endpoints(old, new)

      if (length(changed)) {
        verb <- ifelse(is.null(old), "Adding", "Changing")
        ors_cli(info = list(c("*" = "{verb} the following endpoints:")))
        ors_cli(bullets = list(stats::setNames(
          cli::style_bold(paste("-", changed)),
          rep(" ", length(changed))
        )))
        self$config$parsed$ors$endpoints <- change_endpoints(self, ...)
        self$update()
      }

      invisible(self)
    }
  ),

  # Private ----
  private = list(
    .overwrite = FALSE,
    .verbose = TRUE,
    .prompts = TRUE,
    .alive = TRUE,
    .mount = function() {
      assign("instance", self, envir = ors_cache)
    },
    .parse = function() {
      self$paths <- private$.construct("paths")
      self$extract <- private$.construct("extract")
      self$config <- private$.construct("config")
    },
    .write = function() {
      if (!is.null(self$compose))
        write_dockercompose(self$compose$parsed, self$paths$compose)

      if (!is.null(self$config)) {
        write_config(self$config$parsed, self$paths$config)

        if (private$.is_type("docker")) {
          write_envfile(
            self$config$parsed,
            file.path(self$paths$top, "ors-config.env")
          )
        }
      }
    },
    .construct = function(what, ...) {
      construct <- get0(paste0(".construct_", what))

      if (is.function(construct)) {
        construct(self, private, ...)
      }
    },
    .get_type = function() {
      switch(
        class(self)[[1]],
        ORSDocker = "docker",
        ORSJar = "jar",
        ORSWar = "war",
        "unknown"
      )
    },
    .is_type = function(type) {
      identical(private$.get_type(), type)
    }
  )
)



# External ----

.construct_paths <- function(self, private) {
  # get all easily available paths
  dir <- self$paths$top
  compose_path <- if (private$.is_type("docker")) {
    file.path(dir, "docker-compose.yml")
  }

  # try to find config, otherwise null
  config_path <- detect_config(dir)
  if (is.null(config_path)) {
    config_path <- make_config(dir)
  }

  # try to find extract path, otherwise null
  extract_path <- get_current_extract(dir)

  # construct R representation
  new_ors_paths(dir, extract_path, config_path, compose_path)
}

.construct_compose <- function(self, private, ...) {
  # try to read compose file
  if (!is.null(self$paths$compose)) {
    compose <- read_ors_yaml(self$paths$compose)
  } else {
    return(NULL)
  }

  # parse compose contents
  available_mem <- get_memory_info()
  name <- compose$services$`ors-app`$container_name
  ports <- ports_to_df(compose$services$`ors-app`$ports)
  memory <- read_memory(compose)
  memory <- c(available_mem, memory)
  image <- read_compose_image(compose)
  rebuild <- isTRUE(compose$services$`ors-app`$environment$REBUILD_GRAPHS)

  # enable volumes
  if (is.null(compose$services$volumes)) {
    compose <- configure_volumes(compose)
  }

  # version attribute is obsolete
  compose$version <- NULL

  # ensure access
  if (!is_root()) {
    compose$services$`ors-app`$user <- "1000:1000"
  }

  # set config path
  compose$services$`ors-app`$env_file <- "ors-config.env"

  # construct R representation
  new_ors_compose(name, ports, memory, image, rebuild, compose)
}


.construct_config <- function(self, private, ...) {
  # try to read config file
  if (!is.null(self$paths$config)) {
    config <- read_ors_yaml(self$paths$config)
  } else {
    return(NULL)
  }

  # parse the most important config contents
  profiles <- get_profile_names(config$ors$engine$profiles)

  # construct R representation
  new_ors_config(profiles, config)
}


.construct_extract <- function(self, private, ...) {
  if (is.null(self$paths$extract)) {
    return(NULL)
  }

  # parse extract information
  extract_path <- self$paths$extract
  new_ors_extract(extract_path)
}


new_ors_extract <- function(file) {
  if (is.null(file)) {
    name <- size <- NULL
  } else {
    name <- basename(file)
    size <- round(file.size(file) / 1024 / 1024, 0)
  }
  obj <- list(name = name, size = size)
  class(obj) <- "ors_extract"
  obj
}


new_ors_config <- function(profiles, config) {
  class(config) <- "ors_config_parsed"
  out <- list(
    profiles = profiles,
    parsed = config
  )
  class(out) <- "ors_config"
  out
}


new_ors_compose <- function(name,
                            ports,
                            memory,
                            image,
                            rebuild,
                            parsed) {
  class(parsed) <- "ors_compose_parsed"
  out <- list(
    name = name,
    ports = ports,
    memory = memory,
    image = image,
    rebuild_graphs = rebuild,
    parsed = parsed
  )

  class(out) <- "ors_settings"
  out
}


new_ors_paths <- function(top, extract, config, compose) {
  out <- list(
    top = top,
    extract = extract,
    config = config,
    compose = compose
  )

  class(out) <- "ors_paths"
  out
}


get_ors_release <- function(dir,
                            version = "8.0.0",
                            file = "docker",
                            overwrite = FALSE,
                            verbose = TRUE) {
  dir <- normalizePath(dir, winslash = "/")
  dir <- file.path(dir, sprintf("openrouteservice-%s", version))
  file <- switch(
    file,
    docker = "docker-compose.yml",
    jar = "ors.jar",
    war = "ors.war"
  )

  if (!dir.exists(dir) || overwrite) {
    if (dir.exists(dir) && overwrite) {
      unlink(dir, recursive = TRUE)
    }

    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    url <- "https://github.com/GIScience/openrouteservice/releases/download/%s/%s"

    if (is_version_desc(version, "dh")) {
      version <- "master"
    }

    if (is_numver(version)) {
      if (!minimum_version(version, "8.0.0")) {
        msg = c(
          "!" = "OpenRouteService version must be at least 8.0.0, not {version}",
          "i" = "Older versions are deprecated."
        )
        abort(msg, class = "version_unsupported_error")
      }
      version <- paste0("v", version)
    }

    url <- sprintf(url, version, file)

    ors_cli(progress = list(
      "step",
      msg = "Downloading {file}",
      msg_done = "Downloaded {file}",
      msg_failed = "Failed to download {file}"
    ))

    req <- httr2::request(url)
    withCallingHandlers(
      httr2::req_perform(req, path = file.path(dir, file)),
      httr2_http_404 = function(e) {
        abort(
          "ORS version {version} does not exist.",
          class = "version_404_error"
        )
      }
    )
  } else if (dir.exists(dir)) {
    check_dir_has_file(dir, file)
  }

  ors_cli(info = list(message = c(
    "i" = "Using OpenRouteService version {.field {version}}"
  )))

  dir
}


read_ors_yaml <- function(path, ...) {
  if (is.null(path)) {
    cli::cli_abort(
      c(
        "!" = "Could not find {basename(path)}.",
        "i" = "Is your ORS directory properly initialized?"
      ),
      class = "ors_directory_error"
    )
  }

  yaml::read_yaml(path, ..., readLines.warn = FALSE)
}


create_ors_docker_dirs <- function(dir) {
  ddirs <- c("config", "elevation_cache", "files", "graphs", "logs")
  for (d in ddirs) {
    dir.create(
      file.path(dir, d),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
}


check_dir_has_file <- function(dir, file) {
  if (!file.exists(file.path(dir, file))) {
    exec <- ors_executables()
    exec <- exec[exec %in% list.files(dir)][1]
    alt_type <- switch(
      exec,
      `docker-compose.yml` = "docker",
      `ors.jar` = "jar",
      `ors.war` = "war"
    )
    code <- sprintf("ors_instance(..., type = \"%s\")", alt_type)
    msg <- c(
      "x" = paste(
        "Directory {.path {dir}} already exists but does",
        "not contain a file {.field {file}}."
      ),
      "i" = paste(
        "It does, however, contain a file {.field {exec}}!",
        "Consider using {.code {code}}"
      )
    )
    abort(msg, class = "wrong_ors_type_error")
  }
}


ors_executables <- function() {
  c("docker-compose.yml", "ors.jar", "ors.war")
}
