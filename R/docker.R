ors_up <- function(self, private, wait = TRUE, warn = TRUE, ...) {
  verbose <- private$.verbose
  name <- self$compose$name
  assert_docker_running()

  ors_cli(h2 = "Pulling image")
  pull_ors(self, private)

  cmd <- c(
    "compose", # tool to use
    c("-p", name), # project name
    "-f", file.path(self$paths$compose), # compose file
    "up", "-d", # what to do
    "--no-build", # don't build the image
    c(...) # custom flags
  )

  ors_cli(h2 = "Building container")

  proc <- callr::run(
    command = "docker",
    args = cmd,
    stdout = if (verbose) "|" else NULL,
    stderr = "2>&1",
    stdout_line_callback = cat_callback(verbose),
    error_on_status = FALSE
  )

  assert_process(proc)

  if (wait) {
    ors_cli(cat = "line", h2 = "Setting up service")
    setup_info(verbose)
    notify_when_ready(name, interval = 10L, verbose = verbose, warn = warn)
  }
}


ors_down <- function(self, private) {
  name <- self$compose$name
  assert_docker_running()

  ors_cli(progress = list(
    "step",
    msg = "Taking down container {name}...",
    msg_failed = "Cannot take down container {name}.",
    msg_done = "Successfully took down container {name}.",
    spinner = private$.verbose
  ))

  cmd <- c(
    "compose",
    "-p", name,
    "-f", file.path(self$paths$compose),
    "down"
  )

  proc <- callr::run(
    command = "docker",
    args = cmd,
    stdout = "|",
    stderr = "|",
    error_on_status = FALSE
  )

  assert_process(proc)

  if (grepl("Warning", proc$stderr)) { # nocov start
    cli::cli_warn(strsplit(proc$stderr, ": ")[[1]][2])
    ors_cli(progress = c("done", result = "failed")) # nocov end
  } else {
    ors_cli(progress = c("done", result = "done"))
  }

  self$update()
  private$.mount()
  invisible(self)
}


ors_start <- function(self, private, wait = TRUE) {
  verbose <- private$.verbose
  assert_docker_running()

  name <- self$compose$name

  if (isFALSE(self$is_built())) {
    cli::cli_abort(
      "Container called {.val {name}} does not exist.",
      class = "ors_container_error"
    )
  }

  if (isFALSE(self$is_running())) {
    cmd <- c("start", name)

    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = "|",
      error_on_status = FALSE
    )

    assert_process(proc)

    if (isTRUE(wait)) {
      notify_when_ready(name, interval = 2L, verbose = verbose)
    }
  } else {
    ors_cli(info = list(c("i" = "Container {name} is already running.")))
  }

  self$update()
  private$.mount()
  invisible(self)
}


ors_stop <- function(self, private) {
  name <- self$compose$name
  assert_docker_running()

  if (isTRUE(self$is_running())) {
    ors_cli(progress = list(
      "step",
      msg = "Stopping container...",
      msg_done = "Container stopped.",
      msg_failed = "Cannot stop container.",
      spinner = private$.verbose
    ))

    cmd <- c("stop", name)

    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = "|",
      stderr = "|",
      error_on_status = FALSE
    )

    assert_process(proc)
  } else {
    ors_cli(info = list(c("i" = "Container {name} is already stopped.")))
  }

  self$update()
  private$.mount()
  invisible(self)
}


pull_ors <- function(self, private) {
  verbose <- private$.verbose
  assert_docker_running()

  if (!image_exists()) {
    cmd <- c("pull", self$compose$parsed$services$`ors-app`$image)

    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = if (verbose) "|",
      stderr = if (verbose) "2>&1",
      error_on_status = FALSE,
      spinner = verbose && interactive(),
      encoding = "UTF-8",
      stdout_line_callback = pull_callback(verbose),
    )

    assert_process(proc)
  } else {
    ors_cli(info = list(c("i" = "ORS image already exists.")))
  }

  invisible(self)
}


rm_image <- function(self, private) {
  assert_docker_running()

  if (!container_built(self$compose$name)) {
    cmd1 <- c("images", self$compose$parsed$services$`ors-app`$image, "-q")

    image_ids <- callr::run(
      command = "docker",
      args = cmd1,
      stdout = "|",
      stderr = NULL,
      error_on_status = FALSE
    )

    if (nchar(image_ids$stdout)) {
      ors_cli(progress = list(
        "step",
        msg = "Removing {length(image_ids)} image{?s}...",
        msg_done = "Removed {length(image_ids)} image{?s}.",
        msg_failed = "Cannot remove image."
      ))

      for (id in image_ids) {
        cmd2 <- c("rmi", id)

        proc <- callr::run(
          command = "docker",
          args = cmd2,
          stdout = "",
          stderr = "",
          error_on_status = FALSE
        )

        assert_process(proc)
      }
    } else {
      ors_cli(info = list(c("i" = "No images to remove.")))
    }
  } else {
    cli::cli_abort(
      "Remove the container before removing the image",
      class = "ors_image_container_error"
    )
  }

  invisible(self)
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


cat_callback <- function(verbose) {
  function(newout, proc) {
    ors_cli(cat = newout)
  }
}


pull_callback <- function(verbose) {
  function(newout, proc) {
    exc_list <- c(
      "Download complete", "Downloading", "Extracting", "Waiting",
      "Pulling fs layer", "Verifying Checksum"
    )
    exc_list <- paste(exc_list, collapse = "|")
    exc <- grepl(sprintf(": (%s)", exc_list), newout)
    if (!exc) {
      prc <- grepl("Pull complete", newout)
      if (prc) {
        ors_cli(info = list(c("v" = newout)))
      } else {
        ors_cli(info = list(c("i" = newout)))
      }
    }
  }
}


setup_info <- function(verbose) {
  ors_cli(
    info = list(c("i" = paste(
      "The container is being set up and started now.",
      "You can stop the process now or let it run",
      "and get notified when the service is ready."
    )))
  )
}


# Checks the service status and gives out a visual and audible
# notification when the server is ready. Also watches out for errors
# in the log files.
notify_when_ready <- function(ors_name, interval, warn, verbose) {
  ors_cli(progress = list(
    "step",
    msg = "Starting service",
    msg_done = "Service setup done.",
    msg_failed = "Service setup failed.",
    spinner = verbose
  ))

  proc <- callr::r_bg(
    function(ors_name, watch_for_condition) {
      cond <- NULL
      while (!rors::ors_ready(force = TRUE, id = ors_name)) {
        cond <- watch_for_condition(ors_name)
        if (length(cond$errors)) {
          return(cond)
        }
        Sys.sleep(1L)
      }
      cond
    },
    args = list(ors_name, watch_for_condition),
    package = TRUE
  )

  while (proc$is_alive()) {
    ors_cli(progress = "update")
  }

  cond <- proc$get_result()
  if (warn && !is.null(cond$warn)) {
    cli::cli_warn(cond$warn, class = "ors_setup_warn")
  }

  if (!is.null(cond$errors)) {
    cli::cli_abort(
      c(
        "The service ran into the following errors:",
        cli::cli_vec(cond$errors, style = list(vec_sep = "\n"))
      ),
      call = NULL,
      class = "ors_setup_error"
    )
  }

  if (verbose) {
    notify("ORS Service is ready!")
  }

  invisible(TRUE)
}


#' Searches the OpenRouteService logs for the keyword 'error' and returns
#' their error messages.
#' @noRd
watch_for_condition <- function(ors_name) {
  logs <- docker_logs(ors_name)
  list(
    error = extract_setup_condition(logs, "ERROR"),
    warn = extract_setup_condition(logs, "WARN")
  )
}


extract_setup_condition <- function(logs, type = "ERROR") {
  cond <- grep(type, logs, value = TRUE, fixed = TRUE)
  cond <- regex_match(cli::ansi_strip(cond), "\\[.+\\](.+$)")
  cond <- vapply(cond, "[", 2, FUN.VALUE = character(1))
  if (length(cond)) unique(trimws(cond))
}
