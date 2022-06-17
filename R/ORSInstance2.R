.instance <- function(obj, dir = NULL, extract_path = NULL, config_file = NULL) {
  paths <- .construct_paths(obj, dir, extract_path, config_file)
  compose <- .construct_compose(obj, paths)
  extract <- .construct_extract(paths, extract_path)
  config <- .construct_config(obj, paths)
  status <- .construct_status(obj, compose)
  
  built <- compose$name %in% ls_ors()$name
  
  comp <- list(
    paths = paths,
    extract = extract,
    config = config,
    compose = compose,
    status = status
  )
  
  comp <- lapply(comp, function(x) structure(x[lengths(x) > 0], class = class(x)))
  comp <- comp[lengths(comp) > 0]
  
  structure(
    comp,
    class = "ors_constructor",
    alive = TRUE,
    built = built
  )
}


.construct_paths <- function(obj, dir, extract_path, config_file) {
  dir <- if (length(obj)) obj$paths$dir else dir
  compose_path <- file.path(dir, "docker/docker-compose.yml")
  config_path <- detect_config(dir, config_file)
  
  if (is.null(extract_path)) {
    extract_path <- get_current_extract(compose = yaml::read_yaml(compose_path), dir = dir)
  }
  
  structure(
    list(
      dir = dir,
      extract_path = extract_path,
      config_path = config_path,
      compose_path = compose_path
    ),
    class = "ors_constructor_paths"
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
    class = "ors_constructor_settings",
    update_compose = NULL
  )
}


.construct_config <- function(obj, paths, compose) {
  if (is.null(obj[["config"]])) {
    config <- read_config(paths$config_path)
  } else config <- obj$config$parsed
  
  profiles <- config$ors$services$routing$profiles$active
  
  structure(
    list(
      profiles = profiles,
      parsed = structure(config, class = c("ors_config", "list"))
    ),
    class = "ors_constructor_config",
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
  } else name <- size <- NULL
  
  structure(
    list(
      name = name,
      size = size
    ),
    class = "ors_constructor_extract"
  )
}


.construct_status <- function(obj, compose) {
  if (is.null(obj[["status"]])) {
    status <- get_ors_status(compose$parsed)
  }
  
  structure(
    status,
    class = "ors_constructor_status"
  )
}


#' OpenRouteService instance
#' 
#' @description Creates an OpenRouteService constructor object. A constructor
#' can be used to configure the OpenRouteService instance and start the
#' application. To do this, the constructor object can be modified and
#' complemented using the following functions:
#' 
#' \itemize{
#'  \item Mount an extract with \code{\link[ORSRouting]{ors_extract}}
#'  \item Change the ORS configuration with \code{\link[ORSRouting]{ors_config}}
#'  \item Change container settings with \code{\link[ORSRouting]{ors_settings}}
#'  \item Build and start a container with \code{\link[ORSRouting]{ors_up}}
#'  \item Take a container down with \code{\link[ORSRouting]{ors_down}}
#'  \item Start an existing container with \code{\link[ORSRouting]{ors_start}}
#'  \item Stop a running container with \code{\link[ORSRouting]{ors_stop}}
#'  \item Remove OpenRouteService with \code{\link[ORSRouting]{ors_remove}}
#' }
#' 
#' @param instance Object of class \code{ors_constructor} or \code{NULL}. If 
#' \code{NULL}, creates a new instance from a given directory. Otherwise, the
#' existing instance object is validated and updated. This is especially useful
#' if files were manually changed (e.g., through the use of
#' \code{\link[ORSRouting]{edit_config}}) or if the respective Docker container
#' was manually built or taken down.
#' @param dir Custom OpenRouteService directory. If not specified, the directory
#' will be downloaded to the system's home directory. If a directory called
#' \code{"openrouteservice-{version}"} is present, the download will be skipped.
#' @param version The OpenRouteService version to use. Can either be a version
#' number (e.g. 6.7.0) or \code{"master"}.
#' @param overwrite Whether to overwrite the current OpenRouteService directory
#' if it exists.
#' @param auto_deletion By default, OpenRouteService prevents all profiles from
#' being built on first setup. If \code{FALSE}, disables this behavior. Otherwise,
#' all profiles other than car have to be enabled after the first setup.
#' Defaults to \code{TRUE}, because the OpenRouteService team recommends
#' building graphs for only the car profile in the initial setup.
#' @param verbose If \code{TRUE}, prints informative messages.
#' 
#' @returns Nested list of class \code{ors_constructor}. The object auto-prints
#' additional attributes, that describe the setup state:
#' \describe{
#'  \item{active}{Whether an ORS instance is currently used inside the package
#'  environment.}
#'  \item{alive}{Whether an ORS object refers to an existing instance or if the
#'  instance was removed using \code{\link[ORSROuting]{ors_remove}}.}
#'  \item{built}{Whether an initial setup of an ORS instance was done.}
#' }
#' 
#' @examples 
#' \dontrun{
#' dir <- dir.create("~/test_ors")
#' 
#' # Download and furnish an ORS instance
#' ors <- ors_instance(dir = dir, version = 6.7.0, auto_deletion = FALSE)
#' ors
#' 
#' # Update an ORS instance
#' ors <- ors_instance(ors)
#' }
#' 
#' @family ORS setup functions
#' 
#' @export
ors_instance <- function(
  instance = NULL,
  dir = "~", 
  version = "master",
  overwrite = FALSE,
  auto_deletion = TRUE,
  verbose = TRUE
) {
  if (!docker_installed()) {
    cli::cli_abort("No docker installation could be detected.")
  }
  
  if (is.linux() && !grant_docker_privileges(run = FALSE)) {
    cli::cli_abort(paste("To use {.cls ORSInstance}, Docker needs to be",
                         "accessible as a non-root user. Refer to the",
                         "function {.fn grant_docker_privileges}"))
  }
  
  start_docker()
  
  if (!is.null(instance)) {
    dir <- instance$paths$dir
    instance[["compose"]] <- NULL
    instance[["config"]] <- NULL
    instance[["status"]] <- NULL
  } else {
    dir <- get_ors_release(dir, version, overwrite)
    instance <- list()
  }
  
  if (!auto_deletion) {
    disable_auto_deletion(dir)
  }
  
  attr(instance, "verbose") <- verbose

  instance <- .instance(instance, dir = dir)
  
  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


get_ors_release <- function(dir, version, overwrite) {
  download_url <- if (version == "master") {
    "https://github.com/GIScience/openrouteservice/archive/refs/heads/master.zip"
  } else {
    sprintf(
      "https://github.com/GIScience/openrouteservice/archive/refs/tags/v%s.zip",
      version
    )
  }
  
  cli_abortifnot(dir.exists(dir))
  
  dir <- normalizePath(dir, winslash = "/")
  basedir <- file.path(dir, sprintf("openrouteservice-%s", version))
  
  if (!dir.exists(basedir) || isTRUE(overwrite)) {
    zip_file <- file.path(dir, "openrouteservice.zip")
    
    cli::cli_progress_step(msg = "Downloading service backend from GitHub repository...",
                           msg_done = "Successfully downloaded the service backend.",
                           msg_failed = "Failed to download the service backend.",
                           spinner = interactive())
    proc <- callr::r_bg(function(url, zip) {
      download.file(url, destfile = zip, quiet = TRUE)
    }, args = list(download_url, zip_file))
    while(proc$is_alive()) cli::cli_progress_update()
    if (nchar(proc$read_error())) {
      cli::cli_abort("Cannot download release version {.val {version}}.")
    }
    cli::cli_progress_done()
    
    cli::cli_progress_step(msg = "Extracting files...",
                           msg_done = "Extracted files.",
                           msg_failed = "Could not extract files.",
                           spinner = interactive())
    
    proc <- callr::r_bg(function(zip, dir) {
      unzip(zip, exdir = dir)
    }, args = list(zip_file, dir))
    while(proc$is_alive()) cli::cli_progress_update()
    cli::cli_progress_done()
    
    cli::cli_progress_step(msg = "Removing zip file...",
                           msg_done = "Removed zip file.",
                           msg_failed = "Could not remove zip file.",
                           spinner = interactive())
    
    proc <- callr::r_bg(function(zip) {
      file.remove(zip)
    }, args = list(zip_file))
    while(proc$is_alive()) cli::cli_progress_update()
    cli::cli_progress_done()
  } else {
    cli::cli_alert_info("OpenRouteService directory detected. Download will be skipped.")
  }

  basedir
}


start_docker <- function() {
  if (!docker_running()) {
    if (is.windows()) {
      cli::cli_alert_info(
        "Docker is required to be running in order to start an OpenRouteService instance."
      )
      docker_path <- Sys.which("docker")
      docker_desktop <- file.path(file_path_up(docker_path, 3L),
                                  "Docker Desktop.exe")
      
      status <- file.open(docker_desktop)
      
      # If Docker is installed, it will try to open
      if (status == 0L || is.null(status)) {
        if (interactive()) {
          cli::cli_progress_step(
            "Starting Docker...",
            spinner = TRUE,
            msg_done = "Docker Desktop is now running.",
            msg_failed = "The Docker startup has timed out."
          )
        }
        
        # Check if Docker is usable by running a Docker command
        proc <- callr::r_bg(
          function() {
            while(callr::run("docker", "ps", stdout = NULL, stderr = NULL,
                             error_on_status = FALSE)$status != 0L) {
              Sys.sleep(1L)
            }
          }
        )
        
        while(proc$is_alive()) {
          if (interactive()) cli::cli_progress_update()
          Sys.sleep(0.01)
          difft <- difftime(Sys.time(), proc$get_start_time(), units = "secs")
          if (difft > 180L) cli::cli_abort("Docker startup timed out.")
        }
        
        if (interactive()) cli::cli_progress_done()
      } else {
        cli::cli_abort("Something went wrong while starting Docker. Is it installed?")
      }
    } else if (is.linux()) {
      callr::run(
        command = "systemctl",
        args = c("start", "docker"),
        stdout = NULL,
        stderr = NULL
      )
    }
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

