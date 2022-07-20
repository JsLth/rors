#' Start an ORS container
#' 
#' @description Build and start an OpenRouteService container.
#' 
#' @param wait Logical. If \code{TRUE}, the function will not stop running
#' after the container is being started and will give out a notification as
#' soon as the service is ready. If \code{FALSE}, the function will start
#' the container and then stop. To check the server status, you can then
#' call \code{\link{ors_ready}}.
#' @param distinct Logical. If \code{TRUE}, adds a project name flag which
#' makes it possible to build multiple containers from the same image and
#' compose file. This is useful for running multiple instances at the same time.
#' @inheritParams ors_extract
#' 
#' @returns Nested list of class \code{ors_instance}.
#' 
#' @family ORS setup functions
#' 
#' @export
ors_up <- function(instance, wait = TRUE, verbose = TRUE, distinct = TRUE) {
  if (!instance$status[1]) {
    cli::cli_abort("Docker is not running.")
  }
  
  name <- instance$compose$parsed$services$`ors-app`$container_name
  
  cli::cli_rule(left = "Pulling image")
  pull_ors(instance, verbose = verbose)
  
  cmd <- c(
    "compose",
    if (isTRUE(distinct)) c("-p", shQuote(name)),
    "-f", file.path(instance$paths$dir, "docker/docker-compose.yml"),
    "up", "-d",
    "--no-build"
  )
  
  cat("\n")
  cli::cli_rule(left = "Building container")
  proc <- callr::run(
    command = "docker",
    args = cmd,
    stdout = if (isTRUE(verbose)) "" else NULL,
    stderr = "2>&1",
    error_on_status = FALSE
  )
  
  status <- proc$status
  
  if (!is.na(status) && !identical(status, 0L)) {
    cli::cli_abort(c("The container setup encountered an error.",
                     "Error code {proc$status}: {proc$stderr}"))
  }
  
  if (wait) {
    cat("\n")
    cli::cli_rule(left = "Setting up service")
    notify_when_ready(name, interval = 10L, silently = FALSE)
  }
  
  compose <- set_graphbuilding(NA, instance$compose$parsed)
  instance[["status"]] <- get_ors_status(compose, instance$paths)
  attr(instance, "built") <- TRUE
  
  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' Take down an ORS container
#' 
#' @description Stops and removes an OpenRouteService container. This is useful
#' if you need to rebuild a container, for example if profiles or extract were
#' changed.
#' @inheritParams ors_up
#' 
#' @returns Nested list of class \code{ors_instance}.
#' 
#' @family ORS setup functions
#' 
#' @export
ors_down <- function(instance) {
  if (instance$status[3]) {
    cli::cli_progress_step(
      "Taking down container {instance$compose$name}...",
      msg_failed = "Cannot take down container {instance$compose$name}.",
      msg_done = "Successfully took down container {instance$compose$name}."
    )
    
    name <- instance$compose$parsed$services$`ors-app`$container_name

    cmd <- c(
      "compose",
      "-p", name,
      "-f", file.path(instance$paths$dir, "docker/docker-compose.yml"),
      "down"
    )
    
    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = "|",
      error_on_status = FALSE
    )

    if (!identical(proc$status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       "Error code {proc$status}: {proc$stderr}"))
    } else if (grepl("Warning", proc$stderr)) {
      cli::cli_warn(strsplit(proc$stderr, ": ")[[1]][2])
      cli::cli_progress_done(result = "failed")
    } else cli::cli_progress_done(result = "done")
  } else {
    cli::cli_inform(c("i" = "Container is already down."))
  }
  
  instance[["status"]] <- get_ors_status(compose, instance$paths)
  attr(instance, "built") <- FALSE
  
  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' Start an ORS container
#' 
#' @description Starts an OpenRouteService container. This can only be used to
#' start existing containers. For building a container and setting up the
#' OpenRouteService application, see \code{\link[ORSRouting]{ors_up}}.
#' @inheritParams ors_up
#' 
#' @returns Nested list of class \code{ors_instance}.
#' 
#' @family ORS setup functions
#' 
#' @export
ors_start <- function(instance, wait = TRUE) {
  if (isFALSE(instance$status[3])) {
    cli::cli_abort("Container called {.val {name}} does not exist.")
  }
  
  name <- instance$compose$parsed$services$`ors-app`$container_name
  
  if (isFALSE(instance$status[4])) {
    cmd <- c("start", name)
    
    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = "|",
      error_on_status = FALSE
    )

    if (!identical(proc$status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       "Error code {proc$status}: {proc$stderr}"))
    }
    
    if (isTRUE(wait)) {
      notify_when_ready(name, interval = 2L, silently = TRUE)
      cli::cli_progress_done()
    }
  } else {
    cli::cli_inform(c("i" = "Container {name} is already running."))
  }
  
  compose <- set_graphbuilding(NA, instance$compose$parsed)
  instance[["status"]] <- get_ors_status(compose, instance$paths)
  attr(instance, "built") <- TRUE
  
  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' Stop an ORS container
#' 
#' @description Stops a running OpenRouteService container. This can be used
#' over \code{ors_down}, if changes to the configuration were made that do not
#' require rebuilding the graphs (i.e. everything except profiles).
#' @inheritParams ors_up
#' 
#' @returns Nested list of class \code{ors_instance}.
#' 
#' @family ORS setup functions
#' 
#' @export
ors_stop <- function(instance) {
  name <- instance$compose$parsed$services$`ors-app`$container_name
  
  if (isTRUE(instance$status[4])) {
    cli::cli_progress_step("Stopping container...",
                           msg_done = "Container stopped.",
                           msg_failed = "Cannot stop container.")
    
    cmd <- c("stop", name)
    
    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = "|",
      error_on_status = FALSE
    )
    
    if (!identical(proc$status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error",
                       "Error code {proc$status}: {proc$stderr}"))
    }
    
    cli::cli_progress_done()
  } else {
    cli::cli_inform(c("i" = "Container {name} is already stopped."))
  }
  
  compose <- set_graphbuilding(NA, instance$compose$parsed)
  instance[["status"]] <- get_ors_status(compose, instance$paths)

  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' Make changes to ORS setup
#' 
#' @description Change the configuration, compose or extract of a running
#' OpenRouteService instance. This is a simple convenience function that
#' automates stopping, making changes and restarting the container. To be in
#' full control of this process, take a look at the other setup functions.
#' 
#' @param ... Arguments passed to \code{\link[ORSRouting]{ors_config}},
#' \code{\link[ORSRouting]{ors_settings}} and \code{\link[ORSRouting]{ors_extract}}.
#' @inheritParams ors_up
#' 
#' @returns Nested list of class \code{ors_instance}.
#' 
#' @family ORS setup functions
#' 
#' @export
ors_change <- function(instance, ..., wait = TRUE) {
  dots <- list(...)
  eargs <- dots[names(dots) %in% names(formals(ors_extract))]
  cargs <- dots[names(dots) %in% names(formals(ors_config))]
  sargs <- dots[names(dots) %in% names(formals(ors_settings))]
  full_stop <- any(dots %in% "profiles") || length(eargs)
  
  if (full_stop) ors_down(instance) else ors_stop(instance)
  instance <- do.call(ors_extract, c(instance, eargs))
  instance <- do.call(ors_config, c(instance, sargs))
  instance <- do.call(ors_settings, c(instance, sargs))
  if (full_stop) ors_up(wait) else ors_start(wait)
  
  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' Remove ORS instance
#' 
#' @description Fully remove any trace of an OpenRouteService instance. This is
#' a means of "uninstalling" OpenRouteService. It removes the container,
#' directory, and optionally the corresponding Docker image. It also disables
#' the instance object.
#' 
#' @param ignore_image If \code{TRUE}, does not remove the Docker image. This
#' can be useful, if the image is to be used for other purposes.
#' @inheritParams ors_up
#' 
#' @returns Empty list of class \code{ors_instance}.
#' 
#' @family ORS setup functions
#' 
#' @export
ors_remove <- function(instance, ignore_image = TRUE) {
  if (isFALSE(attr(instance, "active"))) {
    cli::cli_warn("{.cls ORSInstance} is not active.")
    return(invisible())
  }
  
  if (instance$status[3]) ors_down(instance)
  if (isFALSE(ignore_image)) {
    if (instance$status[2]) rm_image(instance, force = FALSE)
  }
  
  if (dir.exists(instance$paths$dir)) {
    cli::cli_progress_step("Removing main directory...",
                           msg_done = "Removed main directory.",
                           msg_failed = "Cannot remove main directory.")
    unlink(instance$paths$dir, recursive = TRUE)
    cli::cli_progress_done()
  }
  
  cli::cli_progress_step("Terminating R6 class...",
                         msg_done = "Terminated R6 class.",
                         msg_failed = "Cannot remove R6 class object.")
  
  rm(instance, envir = ors_cache)
  structure(list(), class = "ors_instance", alive = FALSE)
}


pull_ors <- function(instance, verbose) {
  if (!instance$status[1]) {
    cli::cli_abort("Docker is not running.")
  }
  
  if (!instance$status[2]) {
    cmd <- c("pull", "openrouteservice/openrouteservice")
    
    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = if (verbose) "|",
      stderr = if (verbose) "2>&1",
      error_on_status = FALSE,
      spinner = verbose && interactive(),
      encoding = "UTF-8",
      stdout_line_callback = pull_callback
    )
    
    status <- proc$status
    
    if (!is.na(status) && !identical(status, 0L)) {
      cli::cli_abort(c("The docker command encountered an error.",
                       "Error code {.val {proc$status}}"))
    }
    
  } else {
    cli::cli_inform(c("i" = "ORS image already exists."))
  }
  
  invisible(instance)
}


rm_image <- function(instance, force) {
  if (!instance$status[1]) {
    cli::cli_abort("Docker is not running.")
  }
  
  if (!instance$status[3]) {
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
                           "Error code {.val {proc$status}}"))
        }
      }
      cli::cli_progress_done()
    } else {
      cli::cli_inform(c("i" = "No images to remove."))
    }
  } else {
    cli::cli_abort("Remove the container before removing the image")
  }
  invisible(instance)
}


docker_running <- function() {
  running <- callr::run(
    command = "docker",
    args = "ps",
    stdout = NULL,
    stderr = NULL,
    error_on_status = FALSE
  )
  
  running$status == 0
}


image_exists <- function() {
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
}


container_built <- function(compose, file) {
  container_name <- compose$services$`ors-app`$container_name
  
  cmd <- c(
    "ps",
    "--format",
    "{{ .Label \"com.docker.compose.project.config_files\" }}",
    "-a"
  )
  
  compose_files <- callr::run(
    "docker",
    args = cmd,
    stdout = "|",
    stderr = NULL,
    error_on_status = FALSE
  )
  
  compose_files <- normalizePath(
    unlist(strsplit(compose_files$stdout, "\n")),
    "/",
    mustWork = FALSE
  )

  file %in% compose_files
}


container_running <- function(compose) {
  container_name <- compose$services$`ors-app`$container_name
  
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
}


get_ors_status <- function(compose, file) {
  compose <- compose$parsed
  file <- file$compose_path
  name <- compose$services$`ors-app`$container_name
  
  if (docker <- docker_running()) {
    if (exists <- image_exists()) {
      if (built <- container_built(compose, file)) {
        if (running <- container_running(compose)) {
          ready <- ors_ready(id = name)
        } else ready <- FALSE
      } else ready <- running <- FALSE
    } else ready <- running <- built <- FALSE
  } else ready <- running <- built <- exists <- FALSE

  c(
    "Docker running" = docker, "Image exists" = exists, "Container built" = built,
    "Container running" = running, "Service ready" = ready
  )
}


ls_ors <- function() {
  cmd <- c(
    "container","ls", "-a",
    "--filter", "ancestor=openrouteservice/openrouteservice",
    "--format", paste0("{\"id\":\"{{.ID}}\",", "\"name\":\"{{.Names}}\",",
                       "\"state\":\"{{.State}}\",", "\"running_for\":\"{{.RunningFor}}\",",
                       "\"size\":\"{{.Size}}\",", "\"ports\":\"{{.Ports}}\"}")
  )
  
  proc <- callr::run(
    "docker",
    cmd,
    stdout = "|",
    stderr = NULL,
    error_on_status = FALSE
  )
  
  if (nchar(proc$stdout)) {
    json_list <- strsplit(proc$stdout, "\n")[[1L]]
    ls_list <- lapply(json_list, function(json) {
      pjson <- jsonlite::fromJSON(json)
      as.data.frame(t(pjson))
    })
    ls_df <- do.call(rbind, ls_list)
    ls_df
  } else data.frame()
}



notify_when_ready <- function(ors_name, interval, silently) {
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
    func = function(ors_name, ors_ready, watch_for_error) {
      while(!ors_ready(force = TRUE, id = ors_name)) {
        errors <- watch_for_error(ors_name)
        if (length(errors)) return(errors)
        Sys.sleep(1L)
      }
    },
    args = list(ors_name, ors_ready, watch_for_error),
    package = TRUE
  )
  
  while (proc$is_alive()) {
    if (interactive()) cli::cli_progress_update()
  }

  errors <- proc$get_result()
  if (!is.null(errors)) {
    cli::cli_abort(c(
      "The service ran into the following errors:",
      cli::cli_vec(errors, style = list(vec_sep = "\n"))
    ))
  }
  
  if (interactive()) cli::cli_progress_done()
  
  if (!silently) {
    notify("ORS Service is ready!")
  }
  invisible(TRUE)
}


watch_for_error <- function(ors_name) {
  # Searches the OpenRouteService logs for the keyword 'error' and returns
  # their error messages. If it turns out that tomcat and the local host can
  # raise errors, too, this will have to be overhauled from the get-go.
  cmd <- c("logs", ors_name)
  logs <- callr::run(
    command = "docker",
    args = cmd,
    stdout = "|",
    stderr = "2>&1",
    error_on_status = FALSE,
    encoding = "UTF-8"
  )$stdout
  logs <- strsplit(logs, "\n")[[1]]

  errors <- grep(
    "error|exception",
    logs,
    value = TRUE,
    ignore.case = TRUE
  )
  error_msgs <- do.call(rbind, strsplit(unlist(errors), " - "))
  
  if (is.null(error_msgs)) return()
  
  # CLI logs are formatted differently and are therefore not split
  # by strsplit. If this is the case, just return the whole thing,
  # else return only the messages.
  if (ncol(error_msgs) > 1L) {
    error_msgs <- error_msgs[, 2L]
  }
  
  unique(error_msgs)
}
