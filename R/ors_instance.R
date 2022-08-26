.instance <- function(obj,
                      dir = NULL,
                      server = NULL,
                      extract_path = NULL,
                      config_file = NULL,
                      verbose = TRUE) {
  if (is.null(server)) {
    paths <- .construct_paths(obj, dir, extract_path, config_file)
    compose <- .construct_compose(obj, paths)
    extract <- .construct_extract(paths, paths$extract_path)
    config <- .construct_config(obj, paths)
    status <- .construct_status(obj, compose, paths)

    comp <- list(
      paths = paths,
      extract = extract,
      config = config,
      compose = compose,
      status = status
    )

    comp <- lapply(comp, \(x) structure(x[lengths(x) > 0], class = class(x)))
    comp <- comp[lengths(comp) > 0]

    type <- "local"
    built <- ors_built(paths$dir)
  } else {
    comp <- .construct_server(server)
    type <- if (is_local(server)) "local" else "remote"
    built <- TRUE
  }

  structure(
    comp,
    class = "ors_instance",
    alive = TRUE,
    type = type,
    built = built,
    verbose = verbose
  )
}



.construct_server <- function(server) {
  ready <- ors_ready(id = server)
  info <- if (ready) get_status(server) else NULL
  token <- Sys.getenv("ORS_TOKEN")
  token <- if (nchar(token)) token else NULL

  if (is_ors_api(server) && is.null(token)) {
    cli::cli_abort(c(
      paste(
        "An API token is needed to make requests",
        "to the public OpenRouteService API"),
      "i" = paste(
        "You can safely enter an API key using",
        "the environment variable {.var ORS_TOKEN}"
      )
    ))
  }

  comp <- list(
    url = server,
    info = info,
    ready = ready,
    token = token
  )

  comp <- comp[lengths(comp) > 0]
}



.construct_paths <- function(obj, dir, extract_path, config_file) {
  dir <- if (length(obj)) obj$paths$dir else dir
  compose_path <- file.path(dir, "docker/docker-compose.yml")
  config_path <- detect_config(dir, config_file)

  if (is.null(extract_path)) {
    extract_path <- get_current_extract(
      obj,
      compose = yaml::read_yaml(compose_path),
      dir = dir
    )
  }

  extract_path <- validate_extract(extract_path)

  structure(
    list(
      dir = dir,
      extract_path = extract_path,
      config_path = config_path,
      compose_path = compose_path
    ),
    class = "ors_instance_paths"
  )
}


.construct_compose <- function(obj, paths) {
  if (is.null(obj$compose)) {
    compose <- read_compose(paths$dir)
    available_mem <- get_memory_info()
  } else {
    compose <- obj$compose$parsed
    available_mem <- obj$compose$memory[1:2]
  }

  name <- compose$services$`ors-app`$container_name
  ports <- read_ports(compose)$df
  memory <- read_memory(compose, num = TRUE)
  memory <- c(available_mem, memory)

  graph_building <- read_graphbuilding(compose)

  structure(
    list(
      name = name,
      ports = ports,
      memory = memory,
      graph_building = graph_building,
      parsed = structure(compose, class = c("ors_compose", "list"))
    ),
    class = "ors_instance_settings",
    update_compose = NULL
  )
}


.construct_config <- function(obj, paths) {
  if (is.null(obj[["config"]])) {
    config <- read_config(paths$config_path)
  } else {
    config <- obj$config$parsed
  }

  profiles <- config$ors$services$routing$profiles$active

  structure(
    list(
      profiles = profiles,
      parsed = structure(config, class = c("ors_config", "list"))
    ),
    class = "ors_instance_config",
    update_config = NULL
  )
}


.construct_extract <- function(paths, extract_path) {
  if (!is.null(paths[["extract"]])) {
    extract_path <- paths$extract_path
  }

  if (!is.null(extract_path)) {
    name <- basename(extract_path)
    size <- round(file.size(extract_path) / 1024 / 1024, 0)
  } else {
    name <- size <- NULL
  }

  structure(
    list(
      name = name,
      size = size
    ),
    class = "ors_instance_extract"
  )
}


.construct_status <- function(obj, compose, paths) {
  if (is.null(obj[["status"]])) {
    status <- get_ors_status(compose, paths)
  } else {
    status <- obj$status
  }

  structure(
    status,
    class = "ors_instance_status"
  )
}


#' OpenRouteService instance
#'
#' @description Creates an OpenRouteService instance object. An instance
#' represents either a local/remote server or a directory from which
#' OpenRouteService can be set up. Running this function or any of the related
#' functions listed below stores the instance in an internal environment object
#' and enables functions like \code{\link{ors_distances}} to
#' automatically detect the appropriate server information needed to make a
#' successful request. Hence, this function should always be run after
#' loading \code{ORSRouting} as a means of fixing an instance to the current
#' session.
#'
#' While initializing an instance using an already running server requires no
#' further action, this function family excels at building a local
#' OpenRouteService server from source. Setting up a local server effectively
#' removes any server-side rate limits and allows you to conveniently use the
#' package functions on much larger datasets. For setting up a local server,
#' it is required to build and start a Docker container.
#' To do this, \code{ors_instance}
#' starts Docker (if necessary), downloads and unpacks the OpenRouteService
#' source code and returns an object of class \code{ors_instance} that can be
#' modified and complemented using the following functions:
#'
#' \itemize{
#'  \item Mount an extract with \code{\link{ors_extract}}
#'  \item Change the ORS configuration with \code{\link{ors_config}}
#'  \item Change container settings with \code{\link{ors_settings}}
#'  \item Build and start a container with \code{\link{ors_up}}
#'  \item Take a container down with \code{\link{ors_down}}
#'  \item Start an existing container with \code{\link{ors_start}}
#'  \item Stop a running container with \code{\link{ors_stop}}
#'  \item Remove OpenRouteService with \code{\link{ors_remove}}
#' }
#'
#' @param instance \code{[ors_instance]}
#'
#' If \code{NULL}, creates a new instance from a given directory. Otherwise, the
#' existing instance object is validated, updated and stored. This should be
#' done every time after loading the package. It also comes in handy when making
#' any sort of manual changes to the directory or server, such as changing the
#' configuration file.
#' @param dir \code{[character]}
#'
#' Custom OpenRouteService directory. If not specified, the directory
#' will be downloaded to the system's home directory. If a directory called
#' \code{"openrouteservice-{version}"} is present, the download will be skipped.
#' Ignored if \code{server} is not \code{NULL}.
#' @param server \code{[character]}
#'
#' URL of a server that accepts OpenRouteService requests. This can be a URL
#' to a local or a remote server. The official public API can be accessed using
#' the shortcut \code{"api"}. Keep in mind that the public API is
#' rate-restricted and requests are automatically throttled to 40 requests per
#' minute. Routing functions \emph{will} be slow for larger datasets.
#' @param version \code{[character]}
#'
#' The OpenRouteService version to use. Can either be a version number (e.g.
#' 6.7.0) or \code{"master"}. Ignored if \code{server} is not \code{NULL}.
#' @param overwrite \code{[logical]}
#'
#' Whether to overwrite the current OpenRouteService directory
#' if it exists.
#' @param verbose \code{[logical]}
#'
#' If \code{TRUE}, prints informative messages and spinners.
#'
#' @returns Nested list of class \code{ors_instance}. The object contains all
#' information relevant for a complete server setup, namely paths,
#' configurations, settings, and status helpers. The object auto-prints
#' additional attributes, that describe the setup state:
#' \describe{
#'  \item{active}{Whether an ORS instance is currently used inside the package
#'  environment.}
#'  \item{alive}{Whether an ORS object refers to an existing instance or if the
#'  instance was removed using \code{\link{ors_remove}}.}
#'  \item{built}{Whether an initial setup of an ORS instance was done.}
#'  \item{type}{Whether an instance represents a remote server or a local
#'  server/directory}
#' }
#'
#' @examples
#' \dontrun{
#' dir <- dir.create("~/test_ors")
#'
#' # Download and furnish an ORS instance
#' ors <- ors_instance(dir = dir, version = "6.7.0", auto_deletion = FALSE)
#' ors
#'
#' # Update an ORS instance
#' ors <- ors_instance(ors)
#' }
#'
#' @family ORS setup functions
#'
#' @export
ors_instance <- function(instance = NULL,
                         dir = "~",
                         server = NULL,
                         version = "master",
                         overwrite = FALSE,
                         verbose = TRUE) {
  if (is.character(instance)) {
    cli::cli_abort(c(
      paste(
        "Argument {.var instance} is expected to be of class",
        "{.cls ors_instance}, not {.cls character}."
      ),
      "i" = paste(
        "Did you accidentally pass a directory",
        "without specifying the argument name?"
      )
    ))
  }
  
  local <- attr(instance, "type") == "local"

  if (is.null(server) && local) {
    if (!dir.exists(dir)) {
      cli::cli_abort(
        "{.path {dir}} does not exist and cannot be used as a directory."
      )
    }

    if (!docker_installed()) {
      cli::cli_abort("No docker installation could be detected.")
    }

    if (is_linux() && !grant_docker_privileges(run = FALSE)) {
      cli::cli_abort(paste(
        "To use {.cls ORSInstance}, Docker needs to be",
        "accessible as a non-root user. Refer to the",
        "function {.fn grant_docker_privileges}"
      ))
    }

    start_docker(verbose = verbose)

    if (!is.null(instance)) {
      dir <- instance$paths$dir
      if (!dir.exists(dir)) {
        cli::cli_abort("OpenRouteService directory does not exist.")
      }
      instance[c("compose", "config", "status")] <- NULL
    } else {
      dir <- get_ors_release(dir, version, overwrite, verbose)
      instance <- list()
    }
  } else if (!is.null(server) && local) {
    instance <- list()
    if (identical(server, "api")) server <- "https://api.openrouteservice.org/"
    if (!is_url(server)) {
      cli::cli_abort(
        "{.path {server}} is not a valid URL to an OpenRouteService server"
      )
    }
  } else {
    server <- instance$url
  }

  instance <- .instance(instance, dir = dir, server = server, verbose = verbose)

  attr(instance, "verbose") <- verbose

  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


get_ors_release <- function(dir, version, overwrite, verbose) {
  download_url <- ifelse(
    version == "master",
    "https://github.com/GIScience/openrouteservice/archive/refs/heads/master.zip",
    sprintf(
      "https://github.com/GIScience/openrouteservice/archive/refs/tags/v%s.zip",
      version
    )
  )

  if (!dir.exists(dir)) dir.create(dir)

  dir <- normalizePath(dir, winslash = "/")
  basedir <- file.path(dir, sprintf("openrouteservice-%s", version))

  if (dir.exists(basedir) || !overwrite) {
    zip_file <- file.path(dir, "openrouteservice.zip")

    ors_cli(
      progress = "step",
      msg = "Downloading service backend from GitHub repository...",
      msg_done = "Successfully downloaded the service backend.",
      msg_failed = "Failed to download the service backend.",
      spinner = TRUE
    )

    proc <- callr::r_bg(function(url, zip) {
      utils::download.file(url, destfile = zip, quiet = TRUE)
    }, args = list(download_url, zip_file))
    while (proc$is_alive()) cli::cli_progress_update()
    if (nchar(proc$read_error())) {
      cli::cli_abort("Cannot download release version {.val {version}}.")
    }

    ors_cli(
      progress = "step",
      msg = "Extracting files...",
      msg_done = "Extracted files.",
      msg_failed = "Could not extract files.",
      spinner = TRUE
    )

    proc <- callr::r_bg(function(zip, dir) {
      utils::unzip(zip, exdir = dir)
    }, args = list(zip_file, dir))
    while (proc$is_alive()) ors_cli(progress = "update")

    ors_cli(
      progress = "step",
      msg = "Removing zip file...",
      msg_done = "Removed zip file.",
      msg_failed = "Could not remove zip file.",
      spinner = TRUE
    )

    proc <- callr::r_bg(function(zip) {
      file.remove(zip)
    }, args = list(zip_file))
    while (proc$is_alive()) ors_cli(progress = "update")
  } else {
    ors_cli(info = c(
      "i" = "OpenRouteService directory detected. Download will be skipped."
    ))
  }

  basedir
}


start_docker <- function(verbose = TRUE) {
  if (docker_running()) {
    return(invisible(NULL))
  }

  if (is_windows()) {
    ors_cli(info = c(
      "i" = paste(
        "Docker is required to be running in order to",
        "start an OpenRouteService instance."
      )
    ))

    docker_path <- Sys.which("docker")
    docker_desktop <- file.path(
      file_path_up(docker_path, 3L),
      "Docker Desktop.exe"
    )

    status <- file.open(docker_desktop)

    # If Docker is installed, it will try to open
    if (status == 0L || is.null(status)) {
      ors_cli(
        progress = "step",
        msg = "Starting Docker...",
        spinner = TRUE,
        msg_done = "Docker Desktop is now running.",
        msg_failed = "The Docker startup has timed out."
      )

      # Check if Docker is usable by running a Docker command
      proc <- callr::r_bg(
        function() {
          while (callr::run("docker", "ps",
            stdout = NULL, stderr = NULL,
            error_on_status = FALSE
          )$status != 0L) {
            Sys.sleep(1L)
          }
        }
      )

      while (proc$is_alive()) {
        ors_cli(progress = "update")
        Sys.sleep(0.01)
        difft <- difftime(Sys.time(), proc$get_start_time(), units = "secs")
        if (difft > 180L) cli::cli_abort("Docker startup timed out.")
      }
    } else {
      cli::cli_abort(
        "Something went wrong while starting Docker. Is it installed?"
      )
    }
  } else if (is_linux()) {
    callr::run(
      command = "systemctl",
      args = c("start", "docker"),
      stdout = NULL,
      stderr = NULL
    )
  }
}


disable_auto_deletion <- function(dir) {
  # Don't delete any profiles. Set up every profile at first start.
  dockerfile_path <- file.path(dir, "Dockerfile")

  dockerfile <- readLines(dockerfile_path, warn = FALSE)
  delete_line <- grep("Delete all profiles but car", dockerfile)

  if (length(delete_line) > 0L) {
    lines_to_be_deleted <- c(delete_line, delete_line + 1L, delete_line + 2L)

    pr_line_end <- gsub(
      pattern = " && \\\\",
      replacement = "",
      x = dockerfile[seq(delete_line - 2L, delete_line - 1)]
    )

    dockerfile[seq(delete_line - 2L, delete_line - 1L)] <- pr_line_end
    dockerfile <- paste(dockerfile[-lines_to_be_deleted], collapse = "\n")
    cat(dockerfile, file = dockerfile_path)
  }
}


ors_built <- function(dir) {
  graphs_dir <- file.path(dir, "docker/graphs")
  as.logical(length(dir(graphs_dir)))
}
