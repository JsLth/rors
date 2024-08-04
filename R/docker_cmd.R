#' Takes a docker container up
#' @noRd
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
    setup_info(verbose)
    notify_when_ready(
      name,
      type = "docker",
      interval = 10L,
      verbose = verbose,
      warn = warn
    )
  }
}


#' Takes a docker container down
#' @noRd
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


#' Starts a docker container
#' @noRd
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
      notify_when_ready(
        name,
        type = "docker",
        interval = 2L,
        warn = FALSE,
        verbose = verbose
      )
    }
  } else {
    ors_cli(info = list(c("i" = "Container {name} is already running.")))
  }

  self$update()
  private$.mount()
  invisible(self)
}


#' Stops a docker container
#' @noRd
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


#' Pulls openrouteservice docker image specified in the compose file
#' @noRd
pull_ors <- function(self, private) {
  verbose <- private$.verbose
  assert_docker_running()

  if (!image_exists(self$compose$image)) {
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


#' Removes a docker image
#' @noRd
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


#' Callback function for callr that applies cli formatting to command
#' line output
#' @noRd
cat_callback <- function(verbose) {
  function(newout, proc) {
    ors_cli(verbatim = newout)
  }
}


#' Callback function for callr that processes image pull output of docker
#' and formats it in a way that looks pretty in the R console
#' @noRd
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


#' Setup information message
#' @noRd
setup_info <- function(verbose) {
  ors_cli(
    cat = "line",
    h2 = "Setting up service",
    info = list(c("i" = paste(
      "The container is being set up and started now.",
      "You can stop the process now or let it run",
      "and get notified when the service is ready."
    )))
  )
}
