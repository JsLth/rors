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
  active = list(

    #' @field docker_running Checks if the Docker daemon is running.
    docker_running = function(v) {
      if (missing(v)) {
        private$.docker_running()
      } else {
        cli::cli_abort("`$docker_running` is read only.")
      }
    },

    #' @field image_built Checks if the openrouteservice:latest image exists.
    image_built = function(v) {
      if (missing(v)) {
        private$.image_built()
      } else {
        cli::cli_abort("`$image_built` is read only.")
      }
    },

    #' @field container_exists Checks if the container ors-app exists.
    container_exists = function(v) {
      if (missing(v)) {
        private$.container_exists()
      } else {
        cli::cli_abort("`$container_exists` is read only.")
      }
    },

    #' @field container_running Checks if the the container ors-app is running.
    container_running = function(v) {
      if (missing(v)) {
        private$.container_running()
      } else {
        cli::cli_abort("`$container_running` is read only.")
      }
    },

    #' @field service_ready Checks if the container service is ready to use. If
    #' this field is `TRUE`, the service can be queried and used.
    service_ready = function(v) {
      if (missing(v)) {
        private$.service_ready()
        setwd("..")
      } else {
        cli::cli_abort("`$service_ready` is read only.")
      }
    },

    #' @field error_log Returns all errors from the logs. If `NULL`, no errors
    #' occurred.
    error_log = function(v) {
      if (missing(v)) {
        private$.watch_for_error()
      } else {
        cli::cli_abort("`$error_log` is read only.")
      }
    }
  ),
  public = list(

    #' @description Initializes ORSDockerInterface, starts the Docker daemon
    #' and specifies the port.
    #' @param port Integer scalar. Port that the server should run on.
    initialize = function(port = 8080) {
      if (Sys.info()["sysname"] == "Linux") {
        cli::cli_alert_warning(
          "ORSInstance needs superuser permissions to communicate with Docker"
        )
        system(
          "pkexec env DISPLAY=$DISPLAY XAUTHORITY=$XAUTHORITY ls",
          ignore.stdout = TRUE,
          ignore.stderr = FALSE
        )
      }
      private$.start_docker()
      private$.port <- port
      return(self)
    },

    #' @description Builds the image, starts the container and issues a system
    #' notification when the service is ready to be used.
    #' @param wait Logical. If `TRUE`, the function will not stop running after
    #' the container is being started and will give out a notification as soon
    #' as the service is ready. If `FALSE`, the function will start the
    #' container and then stop. To check the server status, you can then call
    #' `$service_ready` from the class \code{\link{ORSDockerInterface}}.
    image_up = function(wait = TRUE) {
      setwd("docker")
      system(ensure_permission("docker-compose up -d"))
      if (wait) {
        private$.notify_when_ready()
      }
      setwd("..")
    },

    #' @description Deletes the image. Should only be used when the container
    #' does not exist.
    image_down = function() {
      setwd("docker")
      system(ensure_permission("docker-compose down --rmi 'all'"))
      setwd("..")
    },

    #' @description Starts the container and issues a system notification when
    #' the service is ready to be used.
    #' @param wait Logical. If `TRUE`, the function will not stop running after
    #' the container is being started and will give out a notification as soon
    #' as the service is ready. If `FALSE`, the function will start the
    #' container and then stop. To check the server status, you can then call
    #' `$service_ready` from the class \code{\link{ORSDockerInterface}}.
    start_container = function(wait = TRUE) {
      setwd("docker")
      system(ensure_permission("docker start ors-app"), ignore.stdout = TRUE)
      if (wait) {
        private$.notify_when_ready(interval = 2, shutup = TRUE)
      }
      setwd("..")
    },

    #' @description Stops the container.
    stop_container = function() {
      setwd("docker")
      system(ensure_permission("docker stop ors-app"), ignore.stdout = TRUE)
      setwd("..")
    }
  ),
  private = list(
    .port = NULL,
    .service_ready = function(retry = FALSE) {
      health_url <- "http://localhost:%s/ors/health" %>% sprintf(private$.port)
      if (private$.docker_running() &&
         private$.image_built() &&
         private$.container_exists() &&
         private$.container_running()) {
        health <- tryCatch(
          httr::GET(health_url) %>% httr::content(),
          error = function(e) list(status = e)
        )
        identical(health$status, "ready")
      } else {
        FALSE
      }
    },
    .container_exists = function() {
      container_exists <- system(
        ensure_permission(
          "docker ps -a --format \"{{.Names}}\" --filter name=^/ors-app$"
        ),
        intern = TRUE
      ) %>%
        identical("ors-app")
    },
    .container_running = function() {
      container_status <- suppressWarnings(
          paste(
            "docker container ls -a --format \"{{.State}}\"",
            "--filter name=^/ors-app$"
          ) %>%
          ensure_permission() %>%
          system(intern = TRUE, ignore.stderr = TRUE) %>%
          identical("running")
      )
    },
    .image_built = function() {
      image_name <- paste(
        "docker images \"openrouteservice/openrouteservice\"",
        "--format {{.Repository}}"
      ) %>%
      ensure_permission() %>%
      system(intern = TRUE)

      if (length(image_name) > 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    .docker_running = function() {
      docker_check <- system(ensure_permission("docker ps"),
                             ignore.stdout = TRUE,
                             ignore.stderr = TRUE)
      if (docker_check == 0) {
        return(TRUE)
      } else if (docker_check == 1) {
        return(FALSE)
      } else if (docker_check == -1) {
        cli::cli_abort(
          paste(
            "Docker is not recognized as a command.",
            "Is it properly installed?"
          )
        )
      } else {
        cli::cli_abort("Cannot check Docker status for some reason.")
      }
    },
    .start_docker = function() {
      if (!private$.docker_running()) {
        if (Sys.info()["sysname"] == "Windows") {
          docker_path <- system("where docker.exe", intern = TRUE)
          docker_desktop <- docker_path %>%
            strsplit("\\\\") %>%
            unlist() %>%
            head(-3) %>%
            append("Docker Desktop.exe") %>%
            paste(collapse = "/") %>%
            shQuote()
          scode <- shell(docker_desktop, wait = FALSE)
          # If Docker is installed, it will try to open
          if (scode == 0) {
            timer <- 0
            cli::cli_progress_step(
              "Starting Docker...",
              spinner = TRUE,
              msg_done = "Docker Desktop is now running.",
              msg_failed = "The Docker startup has timed out."
            )
            # Check if Docker is usable by running a Docker command
            while (
              system(
                "docker ps",
                ignore.stdout = TRUE,
                ignore.stderr = TRUE
              ) != 0
            ) {
              for (i in 1:100) {
                cli::cli_progress_update()
                Sys.sleep(0.01)
              }
              timer <- timer + 1
              if (timer == 30) {
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
        } else if (Sys.info()["sysname"] == "Linux") {
          system(ensure_permission("systemctl start docker"))
        }
      }
    },
    .notify_when_ready = function(interval = 10, shutup = FALSE) {
      # Checks the service status and gives out a visual and audible
      #' notification when the server is ready. Also watches out for errors
      #' in the log files.
      cli::cli_inform(
        paste(
        "The container is being set up and started now. You can stop the",
        "process now or let it run and get notified when the service is ready."
        )
      )
      cli::cli_progress_step(
        "Starting service",
        spinner = TRUE,
        msg_done = "Service setup done. ORS should not be ready to use.",
        msg_failed = "Service setup failed."
      )
      while (!self$service_ready) {
        for (i in seq_len(interval * 10)) {
          cli::cli_progress_update()
          Sys.sleep(0.1)
        }
        errors <- private$.watch_for_error()
        if (!is.null(errors)) {
          cli::cli_abort(
            c(
              "The service ran into the following errors:",
              cli::cli_vec(errors, style = list(vec_sep = "\n"))
            )
          )
        }
      }
      cli::cli_progress_done()
      if (!shutup) {
        switch(
          Sys.info()["sysname"],
          Windows = {
            system("rundll32 user32.dll, MessageBeep -1")
            system("msg * \"ORS Service is ready!\"")
          },
          Darwin = {
            system(
              paste(
                "osascript -e 'display notification",
                "\"ORS Service is ready!\"",
                "with title",
                "\"Message from R\""
              )
            )
          },
          Linux = {
            system("paplay /usr/share/sounds/freedesktop/stereo/complete.oga")
            system(
              paste(
                "notify-send \"ORS Service is ready!\"",
                "\"Message from R\""
              )
            )
          }
        )
      }
      # TODO: Implement a function that cleans up if an error occurred
      invisible(NULL)
    },
    .watch_for_error = function() {
      # Searches the OpenRouteService logs for the keyword 'error' and returns
      # their error messages. The function might be called before logs are
      # created. If this is the case, don't stop, just don't return anything.
      if (grepl("openrouteservice-master", getwd())) {
        while (basename(getwd()) != "openrouteservice-master") {
          setwd("..")
        }
      } else {
        cli::cli_abort("Wrong directory.")
      }
      if (dir.exists("docker/logs") &&
        is.element(c("ors", "tomcat"), dir("docker/logs")) &&
        length(dir("docker/logs/ors")) != 0 &&
        length(dir("docker/logs/tomcat")) != 0) {
        logs <- c(
          # Logs from Apache Tomcats web container
          readLines("docker/logs/tomcat/catalina.%s.log" %>%
            sprintf(Sys.Date())),
          # Logs from Apache Tomcats local host
          readLines("docker/logs/tomcat/localhost.%s.log" %>%
            sprintf(Sys.Date())),
          # Logs from OpenRouteService (this sometimes stops logging)
          readLines("docker/logs/ors/ors.log"),
          # Output from Dockers logs command (this never stops logging)
          system2(
            "docker",
            args = "logs ors-app",
            stdout = FALSE,
            stderr = TRUE
          )
        )
        error_catcher <- function(log) {
          errors <- grep("error | exception",
          log, value = TRUE,
          ignore.case = TRUE) %>%
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
        setwd("docker")
      }
    }
  ),
  cloneable = FALSE
)
