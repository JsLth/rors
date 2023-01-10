#' Build or take down an ORS container
#'
#' @description Build and start an OpenRouteService container or take it down.
#' These functions are a means to initialize the service (\code{ors_up}) and to
#' rebuild it, e.g., to rebuild routing graphs (\code{ors_down}).
#'
#' @param wait \code{[logical]}
#'
#' If \code{TRUE}, the function will not stop running after the container is
#' being started and will give out a notification as soon as the service is
#' ready. If \code{FALSE}, the function will start the container and then stop.
#' To check the server status, you can then call \code{\link{ors_ready}}.
#' @param ... \code{[character]}
#'
#' Flags to be attached to the \code{docker compose up} command.
#' Notable mentions include \code{--force-recreate} and \code{--no-recreate}.
#' @inheritParams ors_extract
#' @inheritParams ors_image
#'
#' @returns Nested list of class \code{ors_instance}.
#'
#' @family ORS setup functions
#'
#' @export
ors_up <- function(instance, wait = TRUE, tag = "latest", ...) {
  verbose <- attr(instance, "verbose")

  if (!instance$status[1]) {
    cli::cli_abort("Docker is not running.")
  }

  name <- instance$compose$parsed$services$`ors-app`$container_name

  ors_cli(rule = "Pulling image")
  pull_ors(instance, tag)

  cmd <- c(
    "compose", # tool to use
    c("-p", name), # project name
    "-f", file.path(instance$paths$dir, "docker/docker-compose.yml"), # compose file
    "up", "-d", # what to do
    "--no-build", # don't build the image
    c(...) # custom flags
  )

  ors_cli(line = TRUE)
  ors_cli(rule = "Building container")
  proc <- callr::run(
    command = "docker",
    args = cmd,
    stdout = if (isTRUE(verbose)) "" else NULL,
    stderr = "2>&1",
    error_on_status = FALSE
  )

  status <- proc$status

  if (!is.na(status) && !identical(status, 0L)) {
    cli::cli_abort(c(
      "The container setup encountered an error.",
      "Error code {proc$status}: {proc$stderr}"
    ))
  }

  if (wait) {
    ors_cli(line = TRUE)
    ors_cli(rule = "Setting up service")
    notify_when_ready(name, interval = 10L, verbose = verbose)
  }

  instance <- .instance(instance, verbose = verbose)

  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' @rdname ors_up
#'
#' @export
ors_down <- function(instance) {
  verbose <- attr(instance, "verbose")
  ors_cli(
    progress = "step",
    msg = "Taking down container {instance$compose$name}...",
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
    cli::cli_abort(c(
      "The docker command encountered an error",
      "Error code {proc$status}: {proc$stderr}"
    ))
  } else if (grepl("Warning", proc$stderr)) {
    cli::cli_warn(strsplit(proc$stderr, ": ")[[1]][2])
    ors_cli(progress = "done", result = "failed")
  } else ors_cli(progress = "done", result = "done")

  instance <- .instance(instance, verbose = verbose)

  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' Start or stop an ORS container
#'
#' @description Starts or stops an existing ORS container. This can be used
#' to control containers after starting Docker or when making changes to
#' the configuration file. It \emph{cannot} be used to start the service from
#' scratch. To do this, use \code{ors_up}.
#' @inheritParams ors_up
#'
#' @returns Nested list of class \code{ors_instance}.
#'
#' @family ORS setup functions
#'
#' @export
ors_start <- function(instance, wait = TRUE) {
  verbose <- attr(instance, "verbose")
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
      cli::cli_abort(c(
        "The docker command encountered an error",
        "Error code {proc$status}: {proc$stderr}"
      ))
    }

    if (isTRUE(wait)) {
      notify_when_ready(name, interval = 2L, verbose = TRUE)
    }
  } else {
    ors_cli(info = c("i" = "Container {name} is already running."))
  }

  instance <- .instance(instance, verbose = verbose)

  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' @rdname ors_start
#'
#' @export
ors_stop <- function(instance) {
  verbose <- attr(instance, "verbose")
  name <- instance$compose$parsed$services$`ors-app`$container_name

  if (isTRUE(instance$status[4])) {
    ors_cli(
      progress = "step",
      msg = "Stopping container...",
      msg_done = "Container stopped.",
      msg_failed = "Cannot stop container."
    )

    cmd <- c("stop", name)

    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = "|",
      error_on_status = FALSE
    )

    if (!identical(proc$status, 0L)) {
      cli::cli_abort(c(
        "The docker command encountered an error",
        "Error code {proc$status}: {proc$stderr}"
      ))
    }
  } else {
    ors_cli(info = c("i" = "Container {name} is already stopped."))
  }

  instance <- .instance(instance, verbose = verbose)

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
#' @param ... Arguments passed to \code{\link{ors_config}},
#' \code{\link{ors_settings}} and \code{\link{ors_extract}}.
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
  needs_rebuild <- any(dots %in% "profiles") || length(eargs)

  if (needs_rebuild) {
    instance <- ors_down(instance) 
  } else {
    instance <- ors_stop(instance)
  }
  
  instance <- do.call(ors_extract, c(list(instance), eargs))
  instance <- do.call(ors_config, c(list(instance), sargs))
  instance <- do.call(ors_settings, c(list(instance), sargs))
  
  if (needs_rebuild) {
    instance <- ors_up(instance, wait)
  } else {
    instance <- ors_start(instance, wait)
  }

  assign("instance", instance, envir = ors_cache)
  invisible(instance)
}


#' Manage the ORS image
#'
#' @description Pull or remove an OpenRouteService image
#'
#' @param tag \code{[character]}
#'
#' Docker reference tag of the image to pull. Defaults to \code{"latest"}.
#' @param remove \code{[logical]}
#'
#' Whether to pull or remove an image. Cannot pull an image that already exists
#' and cannot remove an image that does not exist. Defaults to pulling.
#' @param force \code{[logical]}
#'
#' Whether to force remove an image. This comes to play, e.g., when a container
#' is still running based on the image to be removed. Defaults to \code{FALSE}.
#' Ignored when \code{remove} is \code{FALSE}.
#' @inheritParams ors_up
#'
#' @returns Nested list of class \code{ors_instance}.
#'
#' @family ORS setup functions
#'
#' @export
ors_image <- function(instance, tag = "latest", remove = FALSE, force = FALSE) {
  if (remove) {
    rm_image(instance, force = force)
  } else {
    pull_ors(instance, tag)
  }
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
  verbose <- attr(instance, "verbose")
  if (isFALSE(attr(instance, "active"))) {
    cli::cli_warn("{.cls ORSInstance} is not active.")
    return(invisible(NULL))
  }

  if (instance$status[3]) ors_down(instance)
  if (isFALSE(ignore_image) && instance$status[2]) {
    rm_image(instance, force = FALSE)
  }

  if (dir.exists(instance$paths$dir)) {
    ors_cli(
      progress = "step",
      msg = "Removing main directory...",
      msg_done = "Removed main directory.",
      msg_failed = "Cannot remove main directory."
    )
    
    unlink(instance$paths$dir, recursive = TRUE)
  }

  if (any_mounted()) {
    is_mounted <- get_id(instance = instance) == get_id()
    if (is_mounted) {
      ors_cli(
        progress = "step",
        msg = "Terminating instance object...",
        msg_done = "Terminated instance object.",
        msg_failed = "Cannot remove instance object."
      )
      
      rm(instance, envir = ors_cache)
    }
  }

  structure(list(), class = "ors_instance", alive = FALSE)
}


pull_ors <- function(instance, tag = "latest") {
  verbose <- attr(instance, "verbose")
  if (!instance$status[1]) {
    cli::cli_abort("Docker is not running.")
  }

  if (!instance$status[2]) {
    cmd <- c("pull", paste0("openrouteservice/openrouteservice:", tag))

    Sys.setenv(ORS_VERBOSE = verbose)
    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = if (verbose) "|",
      stderr = if (verbose) "2>&1",
      error_on_status = FALSE,
      spinner = verbose && interactive(),
      encoding = "UTF-8",
      stdout_line_callback = if (verbose) pull_callback,
    )
    Sys.unsetenv("ORS_VERBOSE")

    status <- proc$status

    if (!is.na(status) && !identical(status, 0L)) {
      cli::cli_abort(c(
        "The docker command encountered an error.",
        "Error code {.val {proc$status}}"
      ))
    }
  } else {
    ors_cli(info = c("i" = "ORS image already exists."))
  }

  invisible(instance)
}


rm_image <- function(instance, force = FALSE) {
  verbose <- attr(instance, "verbose")
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
      ors_cli(
        progress = "step",
        msg = "Removing {length(image_ids)} image{?s}...",
        msg_done = "Removed {length(image_ids)} image{?s}.",
        msg_failed = "Cannot remove image."
      )

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
          cli::cli_abort(c(
            "The docker command encountered an error",
            "Error code {.val {proc$status}}"
          ))
        }
      }
    } else {
      ors_cli(info = c("i" = "No images to remove."))
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
  cmd <- c(
    "images", "openrouteservice/openrouteservice",
    "--format", "{{.Repository}}"
  )

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
    "ps", "-a", "--format",
    "{{ .Label \"com.docker.compose.project.config_files\" }}"
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

  cmd <- c(
    "container", "ls", "-a", "--format", "\"{{.State}}\"",
    "--filter", sprintf("name=^/%s$", container_name)
  )

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
        } else {
          ready <- FALSE
        }
      } else {
        ready <- running <- FALSE
      }
    } else {
      ready <- running <- built <- FALSE
    }
  } else {
    ready <- running <- built <- exists <- FALSE
  }

  c(
    "Docker running" = docker,
    "Image exists" = exists,
    "Container built" = built,
    "Container running" = running,
    "Service ready" = ready
  )
}


ls_ors <- function() {
  cmd <- c(
    "container", "ls", "-a",
    "--filter", "ancestor=openrouteservice/openrouteservice",
    "--format", paste0(
      "{\"id\":\"{{.ID}}\",", "\"name\":\"{{.Names}}\",",
      "\"state\":\"{{.State}}\",", "\"running_for\":\"{{.RunningFor}}\",",
      "\"size\":\"{{.Size}}\",", "\"ports\":\"{{.Ports}}\"}"
    )
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
    ls_df <- tibble::as_tibble(do.call(rbind, ls_list))
    ls_df
  } else {
    tibble::tibble()
  }
}


pull_callback <- function(newout, proc) {
  verbose <- as.logical(Sys.getenv("ORS_VERBOSE"))
  exc_list <- c(
    "Download complete", "Downloading", "Extracting", "Waiting",
    "Pulling fs layer", "Verifying Checksum"
  )
  exc_list <- paste(exc_list, collapse = "|")
  exc <- grepl(sprintf(": (%s)", exc_list), newout)
  if (!exc) {
    prc <- grepl("Pull complete", newout)
    if (prc) {
      ors_cli(info = c("v" = newout))
    } else {
      ors_cli(info = c("i" = newout))
    }
  }
}


notify_when_ready <- function(ors_name, interval, verbose) {
  # Checks the service status and gives out a visual and audible
  # notification when the server is ready. Also watches out for errors
  # in the log files.
  ors_cli(
    info = c("i" = paste(
      "The container is being set up and started now.",
      "You can stop the process now or let it run",
      "and get notified when the service is ready."
    ))
  )

  ors_cli(
    progress = "step",
    msg = "Starting service",
    msg_done = "Service setup done. ORS should now be ready to use.",
    msg_failed = "Service setup failed.",
    spinner = TRUE
  )

  proc <- callr::r_bg(
    func = function(ors_name, ors_ready, watch_for_error) {
      while (!ors_ready(force = TRUE, id = ors_name)) {
        errors <- watch_for_error(ors_name)
        if (length(errors)) {
          return(errors)
        }
        Sys.sleep(1L)
      }
    },
    args = list(ors_name, ors_ready, watch_for_error),
    package = TRUE
  )

  while (proc$is_alive()) {
    ors_cli(progress = "update")
  }

  errors <- proc$get_result()
  if (!is.null(errors)) {
    cli::cli_abort(c(
      "The service ran into the following errors:",
      cli::cli_vec(errors, style = list(vec_sep = "\n"))
    ))
  }

  if (verbose) {
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

  if (is.null(error_msgs)) {
    return()
  }

  # CLI logs are formatted differently and are therefore not split
  # by strsplit. If this is the case, just return the whole thing,
  # else return only the messages.
  if (ncol(error_msgs) > 1L) {
    error_msgs <- error_msgs[, 2L]
  }

  unique(error_msgs)
}
