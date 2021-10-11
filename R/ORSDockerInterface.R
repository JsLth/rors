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
    docker_running = function(arg) {
      if (missing(arg)) {
        docker_check <- auth_system(
          "docker ps",
          stdout = FALSE,
          stderr = FALSE
        )
        docker_check == 0
      } else {
        cli::cli_abort("{.var $docker_running} is read only.")
      }
    },

    #' @field image_built Checks if the openrouteservice:latest image exists.
    image_built = function(arg) {
      if (missing(arg)) {
        image_name <- auth_system(
          paste("docker images \"openrouteservice/openrouteservice\"",
                "--format {{.Repository}}"),
          stdout = TRUE
        )
        length(image_name) > 0
      } else {
        cli::cli_abort("{.var $image_built} is read only.")
      }
    },

    #' @field container_exists Checks if the container ors-app exists.
    container_exists = function(arg) {
      if (missing(arg)) {
        if (self$docker_running) {
          container_status <- auth_system(
            "docker ps -a --format \"{{.Names}}\" --filter name=^/ors-app$",
            stdout = TRUE
          )
          identical(container_status, "ors-app")
        }
      } else {
        cli::cli_abort("{.var $container_exists} is read only.")
      }
    },

    #' @field container_running Checks if the the container ors-app is running.
    container_running = function(arg) {
      if (missing(arg)) {
        if (
          self$docker_running &&
          self$image_built &&
          self$container_exists
        ) {
          container_status <- suppressWarnings(
            auth_system(
              paste("docker container ls -a --format \"{{.State}}\"",
                    "--filter name=^/ors-app$"),
              stdout = TRUE,
              stderr = FALSE
            )
          )
          identical(container_status, "running")
        } else {
          FALSE
        }
      } else {
        cli::cli_abort("{.var $container_running} is read only.")
      }
    },

    #' @field service_ready Checks if the container service is ready to use. If
    #' this field is `TRUE`, the service can be queried and used.
    service_ready = function(arg) {
      if (missing(arg)) {
        if (
          self$docker_running &&
          self$image_built &&
          self$container_exists &&
          self$container_running
        ) {
          ors_ready(force = TRUE)
        } else {
          FALSE
        }
      } else {
        cli::cli_abort("{.var $service_ready} is read only.")
      }
    },

    #' @field error_log Returns all errors from the logs. If `NULL`, no errors
    #' occurred.
    error_log = function(arg) {
      if (missing(arg)) {
        private$.watch_for_error()
      } else {
        if (is.null(arg) || is.na(arg)) {
          logs_dir <- file.path(super$dir, "docker/logs")
          if (dir.exists(logs_dir)) {
            code <- unlink(logs_dir, recursive = TRUE, force = TRUE)
            if (code == 0) {
              private$.watch_for_error()
            } else {
              cli::cli_abort("Logs could not be removed.")
            }
          }
        } else {
          cli::cli_abort(
            paste(
              "Cannot assign logs to {.var $error_log}.",
              "Pass NULL to clear the logs."
            )
          )
        }
      }
    }
  ),
  public = list(

    #' @description Initializes ORSDockerInterface, starts the Docker daemon
    #' and specifies the port.
    #' @param port Integer scalar. Port that the server should run on.
    initialize = function() {
      if (is.linux()) {
        cli::cli_alert_warning(
          "{.cls ORSInstance} needs superuser permissions to communicate with Docker"
        )

        if (!is.rstudio()) {
          system(
            "pkexec env DISPLAY=$DISPLAY XAUTHORITY=$XAUTHORITY ls",
           ignore.stdout = TRUE,
           ignore.stderr = FALSE
          )
        }
      }

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
    image_up = function(wait = TRUE, verbose = TRUE) {
      if (!self$docker_running) {
        cli::cli_abort("Docker is not running.")
      }

      self$error_log <- NULL
      private$.set_port()

      ecode <- auth_system(
        paste("docker compose -f",
              file.path(super$dir, "docker/docker-compose.yml"),
              "up -d"),
        stdout = if (isTRUE(verbose)) "" else FALSE,
        stderr = if (isTRUE(verbose)) "" else FALSE
      )

      if (!is.na(ecode) && ecode != 0) {
        cli::cli_abort(
          c("The container setup encountered an error.",
            paste("Error code", ecode)
          )
        )
      }

      if (wait) {
        private$.notify_when_ready()
      }

      super$setup_settings$graph_building <- NA
    },

    #' @description Deletes the image
    #' @param force If \code{TRUE}, forces the image to removed. If
    #' \code{FALSE}, the command will fail if the compose file does not exist.
    #' Keep \code{TRUE}, if the main directory has a stable path.
    image_down = function(force = TRUE) {
      if (!self$docker_running) {
        cli::cli_abort("Docker is not running.")
      }

      if (!self$container_exists) {

        if (isFALSE(force)) {
          auth_system(paste("docker compose -f",
                            file.path(super$dir, "docker/docker-compose.yml"),
                            "down"))
        } else {
          auth_system(paste("docker rmi --force",
                            "openrouteservice/openrouteservice:latest"))
        }

      } else {
        cli::cli_abort(
          "Remove the container first before taking down the image"
        )
      }
      invisible(NULL)
    },

    #' @description Removes the container after stopping it if necessary.
    remove_container = function() {
      if (self$container_exists) {
        if (self$container_running) {
          self$stop_container()
        }
        auth_system("docker rm ors-app")
        invisible(NULL)
      }
    },

    #' @description Starts the container.
    #' @param wait Logical. If \code{TRUE}, the function will not stop running
    #' after the container is being started and will give out a notification
    #' as soon as the service is ready. If \code{FALSE}, the function will
    #' start the container and then stop. To check the server status, you can
    #' then call \code{$service_ready} or \code{\link{ors_ready}}.
    #'
    start_container = function(wait = TRUE) {
      if (self$container_exists) {
        self$error_log <- NULL

        auth_system("docker start ors-app", stdout = FALSE)

        if (wait) {
          private$.notify_when_ready(interval = 2, silently = TRUE)
        }
      }
    },

    #' @description Stops the container.
    stop_container = function() {
      auth_system("docker stop ors-app", stdout = FALSE)
      invisible(NULL)
    }
  ),

  private = list(

  .set_port = function() {
    port <- super$setup_settings$compose$services$`ors-app`$ports[1] %>%
      strsplit(":") %>%
      unlist() %>%
      unique() %>%
      as.numeric()
    assign("port", port, envir = pkg_cache)
  },

  .start_docker = function() {
      if (!self$docker_running) {
        if (Sys.info()["sysname"] == "Windows") {
          docker_path <- auth_system("where docker.exe", stdout = TRUE)
          docker_desktop <- docker_path %>%
            strsplit("\\\\") %>%
            unlist() %>%
            head(-3) %>%
            append("Docker Desktop.exe") %>%
            paste(collapse = "/") %>%
            shQuote()

          scode <- file.open(docker_desktop)

          # If Docker is installed, it will try to open
          if (scode == 0) {
            timer <- 0
            cli::cli_progress_step(
              "Starting Docker...",
              spinner = interactive(),
              msg_done = "Docker Desktop is now running.",
              msg_failed = "The Docker startup has timed out."
            )

            # Check if Docker is usable by running a Docker command
            while (
              auth_system(
                "docker ps",
                stdout = FALSE,
                stderr = FALSE
              ) != 0
            ) {
              for (i in 1:100) {
                cli::cli_progress_update()
                Sys.sleep(0.01)
              }
              timer <- timer + 1
              if (timer == 180) {
                cli::cli_abort("The Docker startup has timed out.")
              }
            }
          } else if (scode == -1) {
            cli::cli_abort(
              "Docker does not seem to be installed on your system."
            )
          } else {
            cli::cli_abort("Something went wrong while starting Docker.")
          }
        } else if (is.linux()) {
          auth_system("systemctl start docker")
        }
      }
    },

    .notify_when_ready = function(interval = 10, silently = FALSE) {
      # Checks the service status and gives out a visual and audible
      # notification when the server is ready. Also watches out for errors
      # in the log files.
      if (!silently) {
        cli::cli_inform(
          paste(
          "The container is being set up and started now. You can stop the",
          "process now or let it run and get notified when the service is ready."
          )
        )
      }

      cli::cli_progress_step(
        "Starting service",
        spinner = interactive(),
        msg_done = "Service setup done. ORS should now be ready to use.",
        msg_failed = "Service setup failed."
      )

      while (!self$service_ready) {

        for (i in seq_len(interval * 10)) {
          cli::cli_progress_update()
          Sys.sleep(0.1)
        }

        if (!self$container_running) {
          cli::cli_abort("Container was stopped during setup.")
        }

        errors <- private$.watch_for_error()

        if (!is.null(errors)) {
          cli::cli_abort(
            c("The service ran into the following errors:",
              cli::cli_vec(errors, style = list(vec_sep = "\n"))
            )
          )
        }
      }

      cli::cli_progress_done()

      if (!silently) {
        notify("ORS Service is ready.")
      }
      # TODO: Implement a function that cleans up if an error occurred
      invisible(NULL)
    },

    .watch_for_error = function() {
      # Searches the OpenRouteService logs for the keyword 'error' and returns
      # their error messages. The function might be called before logs are
      # created. If this is the case, don't stop, just don't return anything.
      logs_dir <- file.path(super$dir, "docker/logs")

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
          auth_system("docker logs ors-app",
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
          # The function might be called before log files are created. In this
          # case, don't stop, just don't return anything.
      }
    }
  ),
  cloneable = FALSE
)
