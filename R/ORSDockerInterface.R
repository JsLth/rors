# Title     : OpenRouteService Docker Interface
# Objective : Communicate with Docker
# Created by: Jonas Lieth
# Created on: 17.08.2021



#' OpenRouteService Docker interface
#' @description R6 class that interacts with the Docker daemon and is able to
#' run basic commands on the openrouteservice:latest image and the ORS
#' container. \strong{This class is initialized from within
#' \code{\link{ORSInstance}}}.
#' @details If the setup fails, first clean up Docker and the docker directory
#' using \code{$cleanup}.
#'
#' If the setup fails due to an OutOfMemoryError, first check if you allocated
#' enough memory. If it keeps failing, clear the available memory or restart
#' the system.
#' 
#' Sometimes, the the service just refuses to work. In this case, try to reboot
#' your system or wipe the ORS directory using \code{$remove} from
#' \code{\link{ORSInstance}}. This includes, but is not limited to, the following
#' phenomena:
#' \itemize{
#'   \item Cryptic memory errors no matter how much memory you allocate
#'   \item Illegal state exceptions complaining about location indices being
#'         opened with incorrect graphs
#' }
#'
#' @family ORSSetup

ORSDockerInterface <- R6::R6Class(
  classname = "ORSDockerInterface",
  inherit = ORSInstance,
  active = list(

    #' @field docker_running Checks if the Docker daemon is running.
    docker_running = function(arg) ORSDockerInterface$funs$docker_running(arg),

    #' @field image_exists Checks if the openrouteservice:latest image exists.
    #' An alternative image name can be provided.
    image_exists = function(arg) ORSDockerInterface$funs$image_exists(self, arg),

    #' @field container_built Checks if the container ors-app exists.
    #' An alternative container name can be provided.
    container_built = function(arg) ORSDockerInterface$funs$container_built(self, arg),

    #' @field container_running Checks if the the container ors-app is running.
    #' An alternative container name can be provided.
    container_running = function(arg) ORSDockerInterface$funs$container_running(self, arg),

    #' @field service_ready Checks if the container service is ready to use. If
    #' this field is `TRUE`, the service can be queried and used.
    service_ready = function(arg) ORSDockerInterface$funs$service_ready(self, arg),

    #' @field error_log Returns all errors from the logs. If `NULL`, no errors
    #' occurred.
    error_log = function(arg) ORSDockerInterface$funs$error_log(self, private, arg)
  ),

  public = list(

    #' @description Initializes ORSDockerInterface, starts the Docker daemon
    #' and specifies the port.
    #' @param port Integer scalar. Port that the server should run on.
    initialize = function() {
      private$.start_docker()
      self$active <- TRUE
      invisible(self)
    },

    #' @description Builds the container, starts it and issues a system
    #' notification when the service is ready to be used.
    #' @param wait Logical. If \code{TRUE}, the function will not stop running
    #' after the container is being started and will give out a notification as
    #' soon as the service is ready. If \code{FALSE}, the function will start
    #' the container and then stop. To check the server status, you can then
    #' call \code{$service_ready} or \code{\link{ors_ready}}.
    #' @param verbose Logical. If \code{TRUE}, prints Docker logs for container
    #' setup.
    container_up = function(wait = TRUE, verbose = TRUE) ORSDockerInterface$funs$container_up(self, private, wait, verbose),

    #' @description Pulls the openrouteservice/openrouteservice image
    #' @param all_tags Whether to download all tagged images in the repository.
    #' @param verbose Logical. If \code{TRUE}, prints Docker logs for the pull.
    #' Equals the -a tag.
    pull_ors = function(all_tags = FALSE, verbose = TRUE) ORSDockerInterface$funs$pull_ors(self, private, all_tags, verbose),
    
    #' @description Deletes the image.
    #' @param name Optional name of the container. If not provided, the method
    #' will use `getOption("ors_name")` as a default.
    #' @param force Specifies if the image should be forcibly removed, even
    #' if the container is still running or the source directory cannot be
    #' found. Use with caution.
    rm_image = function(force = FALSE) ORSDockerInterface$funs$rm_image(self, force),

    #' @description Removes the container after stopping it if necessary.
    container_down = function() ORSDockerInterface$funs$container_down(self),

    #' @description Starts the container.
    #' @param name Optional name of the container. If not provided, the method
    #' will use `getOption("ors_name")` as a default.
    #' @param wait Logical. If \code{TRUE}, the function will not stop running
    #' after the container is being started and will give out a notification
    #' as soon as the service is ready. If \code{FALSE}, the function will
    #' start the container and then stop. To check the server status, you can
    #' then call \code{$service_ready} or \code{\link{ors_ready}}.
    start_container = function(name = NULL, wait = FALSE) ORSDockerInterface$funs$start_container(self, private, name, wait),

    #' @description Stops the container.
    #' @param name Optional name of the container. If not provided, the method
    #' will use `getOption("ors_name")` as a default.
    stop_container = function(name = NULL) ORSDockerInterface$funs$stop_container(self, name),

    #' @description Takes down the container, removes the image and deletes all
    #' docker directories except /data
    cleanup = function() ORSDockerInterface$funs$cleanup(self)

  ),

  private = list(
    .set_port = function() ORSDockerInterface$funs$set_port(self),
    .start_docker = function() ORSDockerInterface$funs$start_docker(self),
    .pull_callback = function(newout, proc) ORSDockerInterface$funs$pull_callback(newout, proc),
    .notify_when_ready = function(interval, silently) ORSDockerInterface$funs$notify_when_ready(self, private, interval, silently),
    .watch_for_error = function() ORSDockerInterface$funs$watch_for_error(self)
  ),

  cloneable = FALSE
)


ORSDockerInterface$funs <- new.env()

# Public methods --------------------------------------------------------------

ORSDockerInterface$funs$docker_running <- function(arg) {
  if (missing(arg)) {
    cmd <- "ps"

    docker_check <- callr::run(
      command = "docker",
      args = cmd,
      stdout = NULL,
      stderr = NULL,
      error_on_status = FALSE
    )

    identical(docker_check$status, 0L)
  } else {
    cli::cli_abort("{.field $docker_running} is read only.")
  }
}


ORSDockerInterface$funs$image_exists <- function(self, arg) {
  if (!missing(arg)) {
    cli::cli_abort("{.field $image_exists} is read only.")
  }
  
  if (self$docker_running) {
    cmd <- c("images", "openrouteservice/openrouteservice",
              "--format", "{{.Repository}}")

    image_id <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = NULL,
      error_on_status = FALSE
    )
    image_id <- unlist(strsplit(image_id$stdout, "\n"))

    as.logical(length(image_id))
  } else FALSE
}


ORSDockerInterface$funs$container_built <- function(self, arg) {
  if (!missing(arg)) {
    cli::cli_abort("{.field $container_built} is read only.")
  }

  if (self$docker_running && self$image_exists) {
    container_name <- getOption("ors_name", "ors-app")

    cmd <- c("ps", "-a", "--format", "\"{{.Names}}\"", "--filter",
              sprintf("name=^/%s$", container_name))

    container_check <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = NULL,
      error_on_status = FALSE
    )

    grepl(container_name, container_check$stdout)
  } else FALSE
}


ORSDockerInterface$funs$container_running <- function(self, arg) {
  if (!missing(arg)) {
    cli::cli_abort("{.field $container_running} is read only.")
  }

  if (self$docker_running && self$image_exists && self$container_built) {
    container_name <- getOption("ors_name", "ors-app")

    cmd <- c("container", "ls", "-a", "--format", "\"{{.State}}\"",
              "--filter", sprintf("name=^/%s$", container_name))

    container_check <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = NULL,
      error_on_status = FALSE
    )

    grepl("running", container_check$stdout)
  } else FALSE
}


ORSDockerInterface$funs$service_ready <- function(self, arg) {
  if (missing(arg)) {
  }
    if (self$docker_running &&
        self$image_exists &&
        self$container_built &&
        self$container_running) {
          ors_ready(force = TRUE)
    } else FALSE
}


ORSDockerInterface$funs$error_log <- function(self, private, arg) {
  if (missing(arg)) {
    private$.watch_for_error()
  } else {
    if (is.null(arg) || is.na(arg)) {
      logs_dir <- file.path(self$dir, "docker/logs")
      if (dir.exists(logs_dir)) {
        code <- unlink(logs_dir, recursive = TRUE, force = TRUE)
        if (code == 0) {
          private$.watch_for_error()
        } else {
          cli::cli_abort("Logs could not be removed.")
        }
      }
    } else {
      cli::cli_abort(paste("Cannot assign logs to {.var $error_log}.",
                           "Pass NULL to clear the logs."))
    }
  }
}


ORSDockerInterface$funs$pull_ors <- function(self, private, all_tags, verbose) {
  if (!self$docker_running) {
    cli::cli_abort("Docker is not running.")
  }
  
  if (!self$image_exists) {
    cmd <- c("pull", if (isTRUE(all_tags)) "-a", "openrouteservice/openrouteservice")
    
    proc <- callr::process$new(
      command = "docker",
      args = cmd,
      stdout = if (verbose) tempfile(),
      stderr = if (verbose) "2>&1",
      encoding = "UTF-8"
    )
    
    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = if (verbose) "|",
      stderr = if (verbose) "2>&1",
      error_on_status = FALSE,
      spinner = verbose && interactive(),
      encoding = "UTF-8",
      stdout_line_callback = private$.pull_callback
    )
    
    status <- proc$status
    
    if (!is.na(status) && !identical(status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error.",
                       paste("Error code", status)))
    }
    
  } else {
    cli::cli_inform(c("i" = "ORS image already exists."))
  }
  
  invisible()
}


ORSDockerInterface$funs$container_up <- function(self, private, wait, verbose) {
  if (!self$docker_running) {
    cli::cli_abort("Docker is not running.")
  }

  if (!self$container_built) {
    self$error_log <- NULL
  }

  private$.set_port()
  
  old_name <- getOption("ors_name", "ors-app")
  
  ors_name <- self$setup_settings$compose$services$`ors-app`$container_name
  options(ors_name = ors_name)

  cli::cli_rule(left = "Pulling image")
  self$pull_ors(all_tags = FALSE, verbose = verbose)
  
  cmd <- c("compose", "-f",
           file.path(self$dir, "docker/docker-compose.yml"),
           "up", "-d")

  cli::cli_par()
  cli::cli_rule(left = "Building container")
  cat("\n")
  proc <- callr::run(
    command = "docker",
    args = cmd,
    stdout = if (isTRUE(verbose)) "" else NULL,
    stderr = "2>&1"
  )

  status <- proc$status
  
  if (!is.na(status) && !identical(status, 0L)) {
    options(ors_name = old_name)
    
    cli::cli_abort(c("The container setup encountered an error.",
                     paste("Error code", status)))
  }

  if (wait) {
    cli::cli_par()
    cat("\n")
    cli::cli_rule(left = "Setting up service")
    private$.notify_when_ready(interval = 10, silently = FALSE)
  }

  self$setup_settings$graph_building <- NA
  invisible()
}


ORSDockerInterface$funs$rm_image <- function(self, force) {
  if (!self$docker_running) {
    cli::cli_abort("Docker is not running.")
  }

  if (!self$container_built) {
    cmd1 <- c("images", "openrouteservice/openrouteservice", "-q")

    image_ids <- callr::run(
      command = "docker",
      args = cmd1,
      stdout = "|",
      stderr = NULL,
      error_on_status = FALSE
    )

    if (nchar(image_ids$stdout)) {
      cli::cli_progress_step("Removing {length(image_ids)} image{?s}...",
                             msg_done = "Removed {length(image_ids)} image{?s}.",
                             msg_failed = "Cannot remove image.")

      for (id in image_ids) {
        cmd2 <- c("rmi", ifelse(force, "--force", ""), id)

        rmvd <- callr::run(
          command = "docker",
          args = cmd2,
          stdout = "",
          stderr = "",
          error_on_status = FALSE
        )

        if (!identical(rmvd$status, 0L)) {
          cli::cli_abort(c("The docker command encountered an error",
                           "Error code {.val {status}}"))
        }
      }
      cli::cli_progress_done()
    } else {
      cli::cli_inform(c("i" = "No images to remove."))
    }
  } else {
    cli::cli_abort("Remove the container before removing the image")
  }
  invisible()
}


ORSDockerInterface$funs$container_down <- function(self) {
  if (self$container_built) {
    cli::cli_progress_step("Taking down container...",
                           msg_done = "Took down container.",
                           msg_failed = "Cannot take down container.")
    
    cmd <- c("compose", "-f", file.path(self$dir, "docker/docker-compose.yml"), "down")

    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = "|",
      error_on_status = FALSE
    )

    if (!identical(proc$status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       "Error code {.val {status}}"))
    }
    
    cli::cli_progress_done()
  } else {
    cli::cli_inform(c("i" = "Container is already down."))
  }
  invisible()
}


ORSDockerInterface$funs$start_container <- function(self, private, name, wait) {
  if (!is.null(name)) {
    old_name <- getOption("ors_name", "ors-app")
    options(ors_name = name)
    on.exit(options(ors_name = old_name))
  } else {
    name <- getOption("ors_name", "ors-app")
  }

  if (isFALSE(self$container_built)) {
    cli::cli_abort("Container called {.val {name}} does not exist.")
  }

  if (isFALSE(self$container_running)) {

    if (isTRUE(wait)) {
      cli::cli_progress_step("Starting container...",
                             msg_done = "Container is now running.",
                             msg_failed = "Cannot start container.")
    }
    
    self$error_log <- NULL
    
    cmd <- c("start", name)

    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = NULL,
      stderr = NULL,
      error_on_status = FALSE
    )

    if (!identical(proc, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       "Error code {.val {status}}"))
    }

    if (isTRUE(wait)) {
      private$.notify_when_ready(interval = 2L, silently = TRUE)
      cli::cli_progress_done()
    }
  } else {
    cli::cli_inform(c("i" = "Container {.val {name} is already running.}"))
  }
  invisible()
}


ORSDockerInterface$funs$stop_container <- function(self, name) {
  if (!is.null(name)) {
    old_name <- getOption("ors_name", "ors-app")
    options(ors_name = name)
    on.exit(options(ors_name = old_name))
  } else {
    name <- getOption("ors_name", "ors-app")
  }

  if (isTRUE(self$container_running)) {
    cli::cli_progress_step("Stopping container...",
                           msg_done = "Container stopped.",
                           msg_failed = "Cannot stop container.")
    
    cmd <- c("stop", name)

    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = NULL,
      stderr = NULL,
      error_on_status = FALSE
    )

    if (!identical(proc$status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       "Error code {.val {status}}"))
    }
    
    cli::cli_progress_done()
  } else {
    cli::cli_inform(c("i" = "Container {.val {name}} is already stopped."))
  }
  invisible()
}


ORSDockerInterface$funs$cleanup <- function(self) {
  if (!self$service_ready) {
    self$container_down()
    self$rm_image()
    conf_dir <- file.path(self$dir, "docker/conf")
    elev_dir <- file.path(self$dir, "docker/elevation_cache")
    grap_dir <- file.path(self$dir, "docker/graphs")
    
    cli::cli_progress_step("Removing config directory...",
                           msg_done = "Removed config directory.",
                           msg_failed = "Cannot remove config directory.")
    unlink(conf_dir, recursive = TRUE, force = FALSE)
    cli::cli_progress_done()
    cli::cli_progress_step("Removing elevation directory...",
                           msg_done = "Removed elevation directory.",
                           msg_failed = "Cannot remove elevation directory.")
    unlink(elev_dir, recursive = TRUE, force = FALSE)
    cli::cli_progress_done()
    cli::cli_progress_step("Removing graph directory...",
                           msg_done = "Removed graph directory.",
                           msg_failed = "Cannot remove graph directory.")
    unlink(grap_dir, recursive = TRUE, force = FALSE)
    cli::cli_progress_done()
  }
}


# Private methods -------------------------------------------------------------

ORSDockerInterface$funs$set_port <- function(self) {
  port <- self$setup_settings$compose$services$`ors-app`$ports[1L]
  port <- as.numeric(unique(unlist(strsplit(port, ":"))))
  assign("port", port[1], envir = pkg_cache)
}


ORSDockerInterface$funs$start_docker <- function(self) {
  if (!self$docker_running) {
    if (is.windows()) {
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


ORSDockerInterface$funs$pull_callback <- function(newout, proc) {
  exc_list <- c("Download complete", "Downloading", "Extracting", "Waiting",
                "Pulling fs layer", "Verifying Checksum")
  exc_list <- paste(exc_list, collapse = "|")
  exc <- grepl(sprintf(": (%s)", exc_list), newout)
  if (!exc) {
    prc <- grepl("Pull complete", newout)
    if (prc) {
      cli::cli_alert_success(newout)
    } else {
      cli::cli_alert_info(newout)
    }
  }
}


ORSDockerInterface$funs$handle_pull_output <- function(proc) {
  pr_lv <- list()
  tmp <- proc$get_output_file()
  bu.sy <- cli::symbol$bullet
  sb <- cli::cli_status("{bu.sy} Pulling image layers...")
  
  while(proc$is_alive()) {
    lv <- readLines(tmp)
    lvb <- lv[grepl("Using|latest", lv)]
    lv <- lv[grepl(": Pull complete|: Already exists", lv)]
    lve <- lv[grepl("Digest|Status|docker.io", lv)]
    lv <- list(lvb, lv, lve)
    for (i in seq_along(lv)) {
      for (v in lv[[i]]) {
        if (!is.element(v, pr_lv)) {
          if (i != 2 || grepl("Already exists", v)) {
            cli::cli_alert_info(v)
          } else {
            cli::cli_status_clear(id = sb)
            cli::cli_alert_success(v)
            sb <- cli::cli_status("{bu.sy} Pulling image layers...")
          }
        }
        pr_lv <- append(pr_lv, v)
      }
    }
  }
}


ORSDockerInterface$funs$notify_when_ready <- function(self, private, interval, silently) {
  # Checks the service status and gives out a visual and audible
  # notification when the server is ready. Also watches out for errors
  # in the log files.
  if (!silently) {
    cli::cli_inform(
      c("i" = paste(
        "The container is being set up and started now.",
        "You can stop the process now or let it run",
        "and get notified when the service is ready.")
        )
      )
  }

  if (interactive()) {
    cli::cli_progress_step(
      "Starting service",
      spinner = TRUE,
      msg_done = "Service setup done. ORS should now be ready to use.",
      msg_failed = "Service setup failed."
    )
  }
  
  proc <- callr::r_bg(
    func = function(private) {
      while(!ORSRouting::ors_ready(force = TRUE)) {
        errors <- private$.watch_for_error()
        if (length(errors)) return(errors)
        Sys.sleep(1L)
      }
    },
    args = list(private),
    package = TRUE
  )

  while (proc$is_alive()) {
    if (interactive()) cli::cli_progress_update()
  }
  
  errors <- proc$get_result()
  if (!is.null(errors)) {
    cli::cli_abort(c("The service ran into the following errors:",
                     cli::cli_vec(errors, style = list(vec_sep = "\n"))))
  }

  if (interactive()) cli::cli_progress_done()

  if (!silently) {
    notify("ORS Service is ready!")
  }
  invisible(TRUE)
}


ORSDockerInterface$funs$watch_for_error <- function(self) {
  # Searches the OpenRouteService logs for the keyword 'error' and returns
  # their error messages. If it turns out that tomcat and the local host can
  # raise errors, too, this will have to be overhauled from the get-go.
  cmd <- c("logs", getOption("ors_name", "ors-app"))
  logs <- callr::run(
    command = "docker",
    args = cmd,
    stdout = "|",
    stderr = "2>&1",
    error_on_status = FALSE,
    encoding = "UTF-8"
  )$stdout
  logs <- strsplit(logs, "\n")

  errors <- grep(
    "error | exception",
    logs,
    value = TRUE,
    ignore.case = TRUE
  )
  error_msgs <- do.call(rbind, strsplit(unlist(errors), " - "))

  # CLI logs are formatted differently and are therefore not split
  # by strsplit. If this is the case, just return the whole thing,
  # else return only the messages.
  if (length(error_msgs) > 1L) {
    error_msgs <- error_msgs[, 2L]
  }
  
  error_msgs
}


#' @export

print.ORSDockerInterface <- function(x, ...) {
  gl <- list(
    x$docker_running,
    x$image_exists,
    x$container_built,
    x$container_running,
    x$service_ready
  )
  
  gln <- c(
    "Docker running",
    "Image exists",
    "Container built",
    "Container running",
    "Service exist"
  )
  
  gl <- sapply(gl, ifelse, cli::col_green(TRUE), cli::col_red(FALSE))

  gln <- sapply(gln, function(n) {
    paste0(n, strrep("\u00a0", 18 - nchar(n)))
  })
  
  names(gl) <- gln
  
  cli::cli_text("Class\u00a0: {.cls {class(x)}}")
  cli::cli_text("Path\u00a0\u00a0: {x$dir}")
  cat("\n")
  cli::cli_dl(gl)
  cat("\n")
  cli::cli_text("Public methods:")
  print(names(ORSDockerInterface$public_methods))
}

