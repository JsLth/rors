ors_up <- function(self, private, wait = TRUE, ...) {
  verbose <- private$.verbose

  if (!docker_running()) {
    cli::cli_abort("Docker is not running.")
  }

  name <- self$compose$name

  ors_cli(rule = "Pulling image")
  pull_ors(self, private)

  cmd <- c(
    "compose", # tool to use
    c("-p", name), # project name
    "-f", file.path(self$paths$compose), # compose file
    "up", "-d", # what to do
    "--no-build", # don't build the image
    c(...) # custom flags
  )

  ors_cli(line = TRUE)
  ors_cli(rule = "Building container")

  proc <- callr::run(
    command = "docker",
    args = cmd,
    stdout = if (verbose) "|" else NULL,
    stderr = "2>&1",
    stdout_line_callback = cat_callback,
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
    setup_info(verbose)
    notify_when_ready(name, interval = 10L, verbose = verbose)
  }
}


ors_down <- function(self, private) {
  if (!docker_running()) {
    cli::cli_abort("Docker is not running.")
  }

  ors_cli(
    progress = "step",
    msg = "Taking down container {self$compose$name}...",
    msg_failed = "Cannot take down container {self$compose$name}.",
    msg_done = "Successfully took down container {self$compose$name}."
  )

  name <- self$compose$name

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

  if (!identical(proc$status, 0L)) {
    cli::cli_abort(c(
      "The docker command encountered an error",
      "Error code {proc$status}: {proc$stderr}"
    ))
  } else if (grepl("Warning", proc$stderr)) {
    cli::cli_warn(strsplit(proc$stderr, ": ")[[1]][2])
    ors_cli(progress = "done", result = "failed")
  } else ors_cli(progress = "done", result = "done")

  self$update()
  private$.mount()
  invisible(self)
}


ors_start <- function(self, private, wait = TRUE) {
  verbose <- private$.verbose

  if (!docker_running()) {
    cli::cli_abort("Docker is not running.")
  }

  name <- self$compose$name

  if (isFALSE(self$is_built())) {
    cli::cli_abort("Container called {.val {name}} does not exist.")
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

    if (!identical(proc$status, 0L)) {
      cli::cli_abort(c(
        "The docker command encountered an error",
        "Error code {proc$status}: {proc$stderr}"
      ))
    }

    if (isTRUE(wait)) {
      notify_when_ready(name, interval = 2L, verbose = verbose)
    }
  } else {
    ors_cli(info = c("i" = "Container {name} is already running."))
  }

  self$update()
  private$.mount()
  invisible(self)
}


ors_stop <- function(self, private) {
  if (!docker_running()) {
    cli::cli_abort("Docker is not running.")
  }

  name <- self$compose$name

  if (isTRUE(self$is_running())) {
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

  self$update()
  private$.mount()
  invisible(self)
}


pull_ors <- function(self, private) {
  verbose <- private$.verbose

  if (!docker_running()) {
    cli::cli_abort("Docker is not running.")
  }

  if (!image_exists()) {
    cmd <- c("pull", self$compose$parsed$services$`ors-app`$image)

    Sys.setenv(ORS_VERBOSE = verbose)
    proc <- callr::run(
      command = "docker",
      args = cmd,
      stdout = if (verbose) "|",
      stderr = if (verbose) "2>&1",
      error_on_status = FALSE,
      spinner = verbose > 1 && interactive(),
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

  invisible(self)
}


rm_image <- function(self, private) {
  if (!docker_running()) {
    cli::cli_abort("Docker is not running.")
  }

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
      ors_cli(
        progress = "step",
        msg = "Removing {length(image_ids)} image{?s}...",
        msg_done = "Removed {length(image_ids)} image{?s}.",
        msg_failed = "Cannot remove image."
      )

      for (id in image_ids) {
        cmd2 <- c("rmi", id)

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
            "Error code {.val {rmvd$status}}"
          ))
        }
      }
    } else {
      ors_cli(info = c("i" = "No images to remove."))
    }
  } else {
    cli::cli_abort("Remove the container before removing the image")
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


container_image <- function(name) {
  cmd <- c(
    "images", name, "--format", "{{json .}}", "--format",
    paste0(
      "{\"id\":\"{{.ID}}\",\"created_at\":\"{{.CreatedAt}}\",",
      "\"created_since\":\"{{.CreatedSince}}\",",
      "\"repo\":\"{{.Repository}}\",\"tag\":\"{{.Tag}}\",",
      "\"size\":\"{{.Size}}\"}"
    )
  )

  info <- callr::run(
    "docker",
    args = cmd,
    stdout = "|",
    stderr = NULL,
    error_on_status = FALSE
  )

  if (!nzchar(info$stdout)) {
    cli::cli_abort("Container {.val {name}} does not exist.")
  }

  info <- jsonlite::fromJSON(info$stdout)
  info$created_at <- as.POSIXct(info$created_at)
  info
}


container_info <- function(name) {
  cmd <- c(
    "container", "ls", "-a", "--format",
    paste0(
      "{\"id\":\"{{.ID}}\",\"image\":\"{{.Image}}\",",
      "\"names\":\"{{.Names}}\",\"ports\":\"{{.Ports}}\",",
      "\"running_for\":\"{{.RunningFor}}\",\"created_at\":\"{{.CreatedAt}}\",",
      "\"state\":\"{{.State}}\"}"
    ),
    "--filter", sprintf("name=^/%s$", name)
  )

  info <- callr::run(
    "docker",
    args = cmd,
    stdout = "|",
    stderr = NULL,
    error_on_status = FALSE
  )

  if (!nzchar(info$stdout)) {
    cli::cli_abort("Container {.val {name}} does not exist.")
  }

  info <- jsonlite::fromJSON(info$stdout)
  info$created_at <- as.POSIXct(info$created_at)
  info
}


get_docker_logs <- function(name) {
  cmd <- c("logs", name)
  logs <- callr::run(
    "docker",
    args = cmd,
    stdout = "|",
    stderr = "2>&1",
    error_on_status = FALSE
  )

  if (identical(logs$status, 0L)) {
    strsplit(logs$stdout, "\n")[[1]]
  }
}


container_built <- function(name) {
  if (!docker_installed()) return(FALSE)
  cmd <- c(
    "ps", "-a", "--format",
    "{{ .Names }}"
  )

  container_names <- callr::run(
    "docker",
    args = cmd,
    stdout = "|",
    stderr = NULL,
    error_on_status = TRUE
  )

  name %in% strsplit(container_names$stdout, "\n")[[1]]
}


container_running <- function(name) {
  if (!docker_installed()) return(FALSE)
  cmd <- c(
    "container", "ls", "-a", "--format", "\"{{.State}}\"",
    "--filter", sprintf("name=^/%s$", name)
  )

  container_check <- callr::run(
    command = "docker",
    args = cmd,
    stdout = "|",
    stderr = NULL,
    error_on_status = TRUE
  )

  grepl("running", container_check$stdout)
}


cat_callback <- function(newout, proc) {
  cat(newout, "\n")
}


pull_callback <- function(newout, proc) {
  verbose <- as.numeric(Sys.getenv("ORS_VERBOSE"))
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


setup_info <- function(verbose) {
  ors_cli(
    info = c("i" = paste(
      "The container is being set up and started now.",
      "You can stop the process now or let it run",
      "and get notified when the service is ready."
    ))
  )
}


# Checks the service status and gives out a visual and audible
# notification when the server is ready. Also watches out for errors
# in the log files.
notify_when_ready <- function(ors_name, interval, verbose) {
  ors_cli(
    progress = "step",
    msg = "Starting service",
    msg_done = "Service setup done.",
    msg_failed = "Service setup failed.",
    spinner = TRUE
  )

  proc <- callr::r_bg(
    function(ors_name, watch_for_error) {
      while (!ORSRouting::ors_ready(force = TRUE, id = ors_name)) {
        errors <- watch_for_error(ors_name)
        if (length(errors)) {
          return(errors)
        }
        Sys.sleep(1L)
      }
    },
    args = list(ors_name, watch_for_error),
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

  if (verbose > 1) {
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
