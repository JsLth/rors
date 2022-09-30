#' Change the ORS settings
#'
#' @description Allocate memory, manage graph building and set name and ports
#' of an OpenRouteService instance by making changes to the docker-compose file.
#'
#' @param name \code{[character]}
#'
#' Name of the OpenRouteService container.
#' @param ports \code{[numeric/character]}
#'
#' Host port that the container should be running on. Passing \code{NULL}
#' is interpreted as "no change". Passing \code{NA} will assign a random port
#' using the \code{\link{httpuv}} package.
#' @param memory \code{[character]}
#'
#' List of initial and maximum memory values. Initial memory is
#' the memory that is allocated to the Docker container from the start.
#' Maximum memory refers to the memory threshold that a Docker container cannot
#' exceed. If only a single value is passed, initial and maximum memory are
#' assumed to be identical.
#' @param auto_deletion \code{[logical]}
#'
#' By default, OpenRouteService prevents all profiles other than \code{"car"}
#' from being built on first setup. If \code{FALSE}, disables this behavior.
#' Otherwise, all profiles other than car have to be enabled after the first
#' setup. Defaults to \code{TRUE}, because the OpenRouteService team recommends
#' building graphs for only the car profile in the initial setup.
#' @param graph_building \code{[character]}
#'
#' Mode of graph building to be applied. Graphs need to be built for each
#' profile and are required to route with a given profile. Possible values
#' include \code{"build"}, \code{"change"} and \code{NA}. \code{"build"} is used
#' when no graphs are built yet. \code{"change"} needs to be set before mounting
#' a new extract file to an existing setup. \code{NA} disables graph building.
#' @inheritParams ors_extract
#'
#' @returns Nested list of class \code{ors_instance}.
#'
#' @family ORS setup functions
#'
#' @export
ors_settings <- function(instance,
                         name = NULL,
                         ports = NULL,
                         memory = NULL,
                         auto_deletion = FALSE,
                         graph_building = NULL) {
  verbose <- attr(instance, "verbose")
  compose <- instance$compose$parsed
  assert(name, class = "character", len = 1L, null = TRUE)
  assert(ports, class = c("numeric", "character"), len = 1L, null = TRUE)
  assert(memory, class = "numeric", len = c(1, 2), null = TRUE)
  assert(auto_deletion, class = "logical", len = 1L)
  assert(graph_building, class = "logical", null = TRUE)

  if (!is.null(instance$paths$extract_path) && is.null(graph_building)) {
    graphs_dir <- file.path(instance$paths$dir, "docker/graphs")
    if (!length(dir(graphs_dir))) {
      graph_building <- "build"
    }
  }

  if (!is.null(name)) {
    compose$services$`ors-app`$container_name <- name
  }

  if (!is.null(ports)) {
    compose$services$`ors-app`$ports <- format_ports(compose, ports)
  }

  if (length(memory)) {
    compose <- write_memory(compose, instance, memory)
  }

  if (auto_deletion) {
    compose <- protect_config(compose)
  }

  if (!is.null(graph_building)) {
    relative_path <- relative_path(instance$paths$extract_path, file.path(instance$paths$dir, "dir"))
    compose <- set_graphbuilding(graph_building, compose, relative_path)
  }

  write_dockercompose(compose, instance$paths$dir)

  instance[["compose"]] <- NULL

  instance <- .instance(instance, verbose = verbose)

  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


read_compose <- function(dir) {
  yaml::read_yaml(file.path(dir, "docker", "docker-compose.yml"))
}


read_ports <- function(compose) {
  cur_ports <- strsplit(compose$services$`ors-app`$ports, ":")
  cur_ports <- do.call(rbind.data.frame, cur_ports)
  cur_ports_vec <- as.numeric(c(t(cur_ports)))
  names(cur_ports) <- c("host", "docker")
  list(df = cur_ports, vec = cur_ports_vec)
}


format_ports <- function(compose, ports) {
  if (length(ports) == 1L || is.null(ports)) {
    if (is.list(ports)) ports <- unlist(ports)
    ports <- list(ports, NULL, NULL, NULL)
  }

  cur_ports <- read_ports(compose)
  cur_ports_df <- cur_ports$df
  cur_ports_vec <- cur_ports$vec

  ports <- lapply(seq(1L, length(ports)), function(pi) {
    if (is.null(ports[[pi]])) {
      cur_ports_vec[pi]
    } else if (is.na(ports[[pi]])) {
      if (requireNamespace("httpuv")) {
        httpuv::randomPort()
      } else {
        cli::cli_abort("To assign a random port, install the {.pkg httpuv} package.")
      }
    } else {
      ports[[pi]]
    }
  })

  ports <- as.data.frame(matrix(ports, ncol = 2L, nrow = 2L, byrow = TRUE))
  names(ports) <- c("host", "docker")

  list(
    sprintf("%s:%s", ports$host[1L], ports$docker[1L]),
    sprintf("%s:%s", ports$host[2L], ports$docker[2L])
  )
}


read_memory <- function(compose, num, gb = TRUE) {
  java_options <- compose$services$`ors-app`$environment[2L]
  java_mem <- java_mem_chr <- utils::tail(unlist(strsplit(java_options, " ")), 2L)

  if (num) {
    java_mem <- as.numeric(gsub(".*?([0-9]+).*", "\\1", java_mem_chr))

    java_mem <- lapply(c(1, 2), function(mi) {
      if (gb && grepl("[0-9]m", java_mem_chr[mi])) {
        java_mem[mi] / 1000L
      } else if (!gb && grepl("[0-9]g", java_mem_chr[mi])) {
        java_mem[mi] * 1000L
      } else {
        java_mem[mi]
      }
    })
  }

  names(java_mem) <- c("init", "max")

  java_mem
}


write_memory <- function(compose, instance, memory) {
  verbose <- attr(instance, "verbose")
  init <- NULL
  max <- NULL

  if (length(memory) >= 2) {
    init <- memory[[1]] * 1000
    max <- memory[[2]] * 1000
  } else if (length(memory) == 1) {
    max <- memory[[1]] * 1000
  } else {
    if (!is.null(instance$extract$size) && !is.null(instance$config$profiles)) {
      size <- round(instance$extract$size * 0.000001, -2L)
      number_of_profiles <- length(instance$config$profiles) / 1000L

      max <- size * 2.5 * number_of_profiles
      init <- max / 2L
    } else {
      ors_cli(
        warn = paste(
          "Memory allocation options were not changed.",
          "Memory estimation is based on extract size and number of profiles.",
          "Either pass memory specifications explicitly or add extract and",
          "configuration first"
        )
      )
      return(compose)
    }
  }

  compose <- format_memory(compose, init, max)

  gc(verbose = FALSE)

  if (instance$compose$memory$free * 0.8 - max / 1024 <= 0L) {
    ors_cli(
      warn = paste(
        "You are allocating more than your available memory.",
        "Consider lowering the allocated RAM."
      )
    )
  }

  compose
}


format_memory <- function(compose, init, max) {
  java_options <- compose$services$`ors-app`$environment[2L]
  java_mem <- read_memory(compose, num = FALSE)

  init_mem_allocation <- java_mem[1L]
  max_mem_allocation <- java_mem[2L]

  java_options <- gsub(
    init_mem_allocation,
    sprintf("-Xms%sm", format(init, scientific = FALSE)),
    java_options
  )

  java_options <- gsub(
    max_mem_allocation,
    sprintf("-Xmx%sm", format(max, scientific = FALSE)),
    java_options
  )

  compose$services$`ors-app`$environment[2] <- java_options
  compose
}


protect_config <- function(compose) {
  conf_volume <- compose$services$`ors-app`$volumes[5]
  if (grepl(":ro", conf_volume, fixed = TRUE)) {
    compose$services$`ors-app`$volumes[5] <- paste0(conf_volume, ":ro")
  }
  compose
}


read_graphbuilding <- function(compose) {
  build <- is.element("build", names(compose$services$`ors-app`))
  change <- !is.na(compose$services$`ors-app`$volumes[6L])
  gb <- compose$services$`ors-app`$environment[1]
  gb <- as.logical(unlist(strsplit(gb, "="))[2])

  if (change) {
    if (gb) "change" else NA
  } else if (build) {
    "build"
  } else {
    NA
  }
}


set_graphbuilding <- function(mode, compose, extract_path) {
  # First, turn off everything
  compose <- force_graphbuilding(FALSE, compose)
  compose$services$`ors-app`$build <- NULL
  compose$
    services$
    `ors-app`$
    volumes <- compose$services$`ors-app`$volumes[-6L]

  # Then, turn things on selectively
  if (identical(mode, "build") && !missing(extract_path)) {
    compose <- force_graphbuilding(TRUE, compose)
    build_branch <- list(
      build = list(
        context = "../",
        args = list(
          ORS_CONFIG = "./docker/data/ors-config.json",
          OSM_FILE = sprintf("./%s", extract_path)
        )
      )
    )

    compose$services$`ors-app` <- append(
      compose$services$`ors-app`,
      build_branch,
      after = 3L
    )
  } else if (identical(mode, "change")) {
    compose <- force_graphbuilding(TRUE, compose)
    change_node <- sprintf("./%s:/ors-core/data/osm_file.pbf", extract_path)

    compose$services$`ors-app`$volumes[6L] <- change_node
  }

  compose
}


force_graphbuilding <- function(on, compose) {
  on <- capitalize_char(on)
  build_graphs_string <- sprintf("BUILD_GRAPHS=%s", on)
  compose$services$`ors-app`$environment[1L] <- build_graphs_string
  compose
}


write_dockercompose <- function(compose, dir) {
  java_opts <- compose$services$`ors-app`$environment[2L]
  catalina_opts <- compose$services$`ors-app`$environment[3L]
  user <- compose$services$`ors-app`$user

  # Put string options in quadruple quotes
  compose$
    services$
    `ors-app`$
    environment[2L] <- shQuote(java_opts, type = "cmd")

  compose$
    services$
    `ors-app`$
    environment[3L] <- shQuote(catalina_opts, type = "cmd")

  compose$
    services$
    `ors-app`$
    user <- shQuote(user, type = "cmd")

  # Build yaml with indented bullet points
  yml_as_string <- yaml::as.yaml(compose, indent.mapping.sequence = TRUE)

  # Remove single quotes that are somehow added by as.yaml when introducing
  # double quotes.
  corrected_yml_string <- gsub("'\"|\"'", "\"", yml_as_string)

  # Remove line breaks of long strings
  corrected_yml_string <- gsub("\\n\\s{8}-", " -", corrected_yml_string)

  # Write new yaml to old yaml file
  cat(corrected_yml_string, file = file.path(dir, "docker/docker-compose.yml"))
}
