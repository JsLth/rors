# Title     : OpenRouteService Docker Interface
# Objective : Communicate with Docker
# Created by: Jonas Lieth
# Created on: 17.08.2021



#' OpenRouteService Docker interface
#' @description R6 class that interacts with the Docker daemon and is able to
#' run basic commands on the openrouteservice:latest image and the ors-app
#' container
#'
#' @importFrom magrittr %>%

ORSDockerInterface <- R6::R6Class(
  classname = "ORSDockerInterface",
  inherit = ORSInstance,
  active = list(

    #' @field docker_running Checks if the Docker daemon is running.
    docker_running = function(arg) ORSDockerInterface$funs$docker_running(arg),

    #' @field image_built Checks if the openrouteservice:latest image exists.
    image_built = function(arg) ORSDockerInterface$funs$image_built(self, arg),

    #' @field container_exists Checks if the container ors-app exists.
    container_exists = function(arg) ORSDockerInterface$funs$container_exists(self, arg),

    #' @field container_running Checks if the the container ors-app is running.
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
      invisible(self)
    },

    #' @description Builds the image, starts the container and issues a system
    #' notification when the service is ready to be used.
    #' @param wait Logical. If \code{TRUE}, the function will not stop running
    #' after the container is being started and will give out a notification as
    #' soon as the service is ready. If \code{FALSE}, the function will start
    #' the container and then stop. To check the server status, you can then
    #' call \code{$service_ready} or \code{\link{ors_ready}}.
    #' @param verbose Logical. If \code{TRUE}, prints Docker logs for container
    #' setup.
    image_up = function(wait = TRUE, verbose = TRUE) ORSDockerInterface$funs$image_up(self, private, wait, verbose),

    #' @description Deletes the image.
    #' @param force Specifies if the image should be forcibly removed, even
    #' if the container is still running or the source directory cannot be
    #' found. Use with caution.
    rm_image = function(force = FALSE) ORSDockerInterface$funs$rm_image(self, force),

    #' @description Removes the container after stopping it if necessary.
    container_down = function() ORSDockerInterface$funs$container_down(self),

    #' @description Starts the container.
    #' @param wait Logical. If \code{TRUE}, the function will not stop running
    #' after the container is being started and will give out a notification
    #' as soon as the service is ready. If \code{FALSE}, the function will
    #' start the container and then stop. To check the server status, you can
    #' then call \code{$service_ready} or \code{\link{ors_ready}}.
    start_container = function(wait) ORSDockerInterface$funs$start_container(self, private, wait),

    #' @description Stops the container.
    stop_container = function() ORSDockerInterface$funs$stop_container(self)

  ),

  private = list(
    .set_port = function() ORSDockerInterface$funs$set_port(self),
    .start_docker = function() ORSDockerInterface$funs$start_docker(self),
    .notify_when_ready = function(interval, silently) ORSDockerInterface$funs$notify_when_ready(self, private, interval, silently),
    .watch_for_error = function() ORSDockerInterface$funs$watch_for_error(self)
  ),

  cloneable = FALSE
)


ORSDockerInterface$funs <- new.env()


ORSDockerInterface$funs$docker_running <- function(arg) {
  if (missing(arg)) {
    cmd <- "ps"

    docker_check <- system2(command = "docker",
                            args = cmd,
                            stdout = FALSE,
                            stderr = FALSE)

    identical(docker_check, 0L)
  } else {
    cli::cli_abort("{.var $docker_running} is read only.")
  }
}


ORSDockerInterface$funs$image_built <- function(self, arg) {
  if (missing(arg)) {
    if (self$docker_running) {
      cmd <- c("images \"openrouteservice/openrouteservice\"",
               "--format {{.Repository}}")

      image_name <- system2(command = "docker",
                            args = cmd,
                            stdout = TRUE)

      length(image_name) > 0
    }
  } else {
    cli::cli_abort("{.var $image_built} is read only.")
  }
}


ORSDockerInterface$funs$container_exists <- function(self, arg) {
  if (missing(arg)) {
    if (self$docker_running && self$image_built) {
      cmd <- c("ps -a",
               "--format \"{{.Names}}\"",
               "--filter name=^/ors-app$")

      container_status <- system2(command = "docker",
                                  args = cmd,
                                  stdout = TRUE)

      identical(container_status, "ors-app")
    }
  } else {
    cli::cli_abort("{.var $container_exists} is read only.")
  }
}


ORSDockerInterface$funs$container_running <- function(self, arg) {
  if (missing(arg)) {
    if (self$docker_running && self$image_built && self$container_exists) {
      cmd <- c("container ls -a",
               "--format \"{{.State}}\"",
               "--filter name=^/ors-app$")

      container_status <- suppressWarnings(system2(command = "docker",
                                                   args = cmd,
                                                   stdout = TRUE,
                                                   stderr = FALSE))

      identical(container_status, "running")
    } else {
      FALSE
    }
  } else {
    cli::cli_abort("{.var $container_running} is read only.")
  }
}


ORSDockerInterface$funs$service_ready <- function(self, arg) {
  if (missing(arg)) {
    if (self$docker_running &&
        self$image_built &&
        self$container_exists &&
        self$container_running) {
          ors_ready(force = TRUE)
    } else {
      FALSE
    }
  } else {
    cli::cli_abort("{.var $service_ready} is read only.")
  }
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


ORSDockerInterface$funs$image_up <- function(self, private, wait, verbose) {
  if (!self$docker_running) {
    cli::cli_abort("Docker is not running.")
  }

  self$error_log <- NULL
  private$.set_port()

  cmd <- c("compose -f",
           file.path(self$dir, "docker/docker-compose.yml"),
           "up -d")

  status <- system2(command = "docker",
                    args = cmd,
                    stdout = if (isTRUE(verbose)) "" else FALSE,
                    stderr = if (isTRUE(verbose)) "" else FALSE)

  if (!is.na(status) && !identical(status, 0L)) {
    cli::cli_abort(c("The container setup encountered an error.",
                     paste("Error code", status)))
  }

  if (wait) {
    private$.notify_when_ready(interval = 10, silently = FALSE)
  }

  self$setup_settings$graph_building <- NA
  invisible()
}


ORSDockerInterface$funs$rm_image <- function(self, force) {
  if (!self$docker_running) {
    cli::cli_abort("Docker is not running.")
  }

  if (!self$container_exists) {

    cmd <- c("rmi",
             ifelse(force, "--force", ""),
             "openrouteservice/openrouteservice:latest")

    status <- system2(command = "docker", args = cmd)

    if (!identical(status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       paste("Error code", status)))
    }

  } else {
    cli::cli_abort(
      "Remove the container before removing the image"
    )
  }
  invisible()
}


ORSDockerInterface$funs$container_down <- function(self) {
  if (self$container_exists) {

    cmd <- c("compose",
             "-f",
             file.path(self$dir, "docker/docker-compose.yml"),
             "down")

    status <- system2(command = "docker", args = cmd)

    if (!identical(status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       paste("Error code", status)))
    }
  }
  invisible()
}


ORSDockerInterface$funs$start_container <- function(self, private, wait) {
  if (self$container_exists) {
    self$error_log <- NULL

    cmd <- "start ors-app"

    system2(command = "docker", args = cmd, stdout = FALSE)

    if (!identical(status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       paste("Error code", status)))
    }

    if (wait) {
      private$.notify_when_ready(interval = 2, silently = TRUE)
    }
  }
  invisible()
}


ORSDockerInterface$funs$stop_container <- function(self) {
  if (self$container_running) {
    cmd <- "stop ors-app"

    status <- system2(command = "docker", args = cmd, stdout = FALSE)

    if (!identical(status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       paste("Error code", status)))
    }
  }
  invisible()
}


ORSDockerInterface$funs$set_port <- function(self) {
  port <- self$setup_settings$compose$services$`ors-app`$ports[1] %>%
    strsplit(":") %>%
    unlist() %>%
    unique() %>%
    as.numeric()
  assign("port", port, envir = pkg_cache)
}


ORSDockerInterface$funs$start_docker <- function(self) {
  if (!self$docker_running) {
    if (Sys.info()["sysname"] == "Windows") {
      docker_path <- Sys.which("docker")
      docker_desktop <- file.path(file_path_up(docker_path, 3),
                                  "Docker Desktop.exe")

      status <- file.open(shQuote(docker_desktop))

      # If Docker is installed, it will try to open
      if (status == 0) {
        cli::cli_progress_step("Starting Docker...",
                                       spinner = TRUE,
                               msg_done = "Docker Desktop is now running.",
                               msg_failed = "The Docker startup has timed out.")

        # Check if Docker is usable by running a Docker command
        while (system2(command = "docker",
                       args = "ps",
                       stdout = FALSE,
                       stderr = FALSE,
                       timeout = 180) != 0) {

          for (i in 1:100) {
            cli::cli_progress_update()
            Sys.sleep(0.01)

          }
        }
      } else {
        cli::cli_abort("Something went wrong while starting Docker.")
      }
    } else if (is.linux()) {
      system2(command = "systemctl", args = "start docker")
    }
  }
}


ORSDockerInterface$funs$notify_when_ready <- function(self, private, interval, silently) {
  # Checks the service status and gives out a visual and audible
  # notification when the server is ready. Also watches out for errors
  # in the log files.
  if (!silently) {
    cli::cli_inform(paste("The container is being set up and started now.",
                          "You can stop the process now or let it run and get",
                          "notified when the service is ready."))
  }

  cli::cli_progress_step("Starting service",
                         spinner = TRUE,
                         msg_done = "Service setup done. ORS should now be ready to use.",
                         msg_failed = "Service setup failed.")

  while (!self$service_ready) {
    for (i in seq_len(interval * 10)) {
      cli::cli_progress_update()
      Sys.sleep(0.1)
    }
    errors <- private$.watch_for_error()
    if (!is.null(errors)) {
      cli::cli_abort(c("The service ran into the following errors:",
                       cli::cli_vec(errors, style = list(vec_sep = "\n"))))
    }
  }

  cli::cli_progress_done()

  if (!silently) {
    notify("ORS Service is ready.")
  }
  # TODO: Implement a function that cleans up if an error occurred
  invisible()
}


ORSDockerInterface$funs$watch_for_error <- function(self) {
  # Searches the OpenRouteService logs for the keyword 'error' and returns
  # their error messages. The function might be called before logs are
  # created. If this is the case, don't stop, just don't return anything.
  logs_dir <- file.path(self$dir, "docker/logs")

  if (dir.exists(logs_dir) &&
      is.element(c("ors", "tomcat"), dir(logs_dir)) &&
      length(dir(file.path(logs_dir, "ors"))) != 0 &&
      length(dir(file.path(logs_dir, "tomcat"))) != 0) {
    log_date <- file.info(file.path(logs_dir, "ors/ors.log"))$ctime %>%
      format(tz = "UTC") %>%
      as.Date()
    logs <- c(
      # Logs from Apache Tomcats web container
      tryCatch(
        expr = readLines(file.path(logs_dir, "tomcat/catalina.%s.log" %>%
          sprintf(log_date))),
        error = function(e) "Log not available"
      ),
      # Logs from Apache Tomcats local host
      tryCatch(
        expr = readLines(file.path(logs_dir, "tomcat/localhost.%s.log" %>%
          sprintf(log_date))),
        error = function(e) "Log not available"
      ),
      # Logs from OpenRouteService (this sometimes stops logging)
      tryCatch(
        expr = readLines(file.path(logs_dir, "ors/ors.log")),
        error = function(e) "Log not available"
      ),
      # Output from Dockers logs command (this never stops logging)
      system2("docker",
              args = "logs ors-app",
              stdout = FALSE,
              stderr = TRUE)
    )

    error_catcher <- function(log) {
      errors <- grep(
        "error | exception",
        log,
        value = TRUE,
        ignore.case = TRUE
      ) %>%
        unlist()
      error_msgs <- errors %>%
        strsplit(" - ") %>%
        do.call(rbind, .)

      # CLI logs are formatted differently and are therefore not split
      # by strsplit. If this is the case, just return the whole thing,
      # else return only the messages.
      if (length(error_msgs) > 1) {
        return(error_msgs[, 2])
      } else {
        return(error_msgs)
      }
    }

    sapply(logs, error_catcher) %>%
      purrr::discard(sapply(., is.null)) %>%
      unlist(use.names = FALSE) %>%
      unique() # Don't return the same errors multiple times
  }
}