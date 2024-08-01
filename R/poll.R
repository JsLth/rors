#' Checks the service status and gives out a visual and audible
#' notification when the server is ready. Also watches out for errors
#' in the log files.
#' @noRd
notify_when_ready <- function(src,
                              type,
                              interval = 10L,
                              warn = TRUE,
                              verbose = TRUE,
                              log_dir = NULL) {
  ors_cli(progress = list(
    "step",
    msg = "Starting service",
    msg_done = "Service setup done.",
    msg_failed = "Service setup failed.",
    spinner = verbose
  ))

  proc <- callr::r_bg(
    function(src, type, log_dir = NULL, watch_for_condition) {
      cond <- NULL
      if (is.null(log_dir)) {
        log_dir <- src
      }
      # while ors service is not ready, keep watching for errors
      while (!rors::ors_ready(force = TRUE, id = src)) {
        cond <- watch_for_condition(log_dir, type = type)
        if (length(cond$error)) {
          return(cond)
        }
        Sys.sleep(1L)
      }
      cond
    },
    args = list(src, type, log_dir, watch_for_condition),
    package = TRUE
  )

  while (proc$is_alive()) {
    ors_cli(progress = "update")
  }

  cond <- proc$get_result()
  if (warn && !is.null(cond$warn)) {
    cli::cli_warn(cond$warn, class = "ors_setup_warn")
  }

  if (!is.null(cond$error)) {
    cli::cli_abort(
      c(
        "The service ran into the following errors:",
        cli::cli_vec(cond$error, style = list(vec_sep = "\n"))
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


#' Takes ORS logs and returns all warnings and errors
#' @noRd
watch_for_condition <- function(src, type = "docker") {
  logs <- switch(
    type,
    docker = docker_logs(src),
    jar = read_logs(src)
  )
  list(
    error = extract_setup_condition(logs, "ERROR"),
    warn = extract_setup_condition(logs, "WARN")
  )
}


#' Takes ORS logs and extracts all conditions of type `type`.
#' @noRd
extract_setup_condition <- function(logs, type = "ERROR") {
  cond <- grep(type, logs, value = TRUE, fixed = TRUE)
  cond <- regex_match(cli::ansi_strip(cond), "\\[.+\\](.+$)")
  cond <- vapply(cond, "[", 2, FUN.VALUE = character(1))
  if (length(cond)) {
    cond <- unique(trimws(cond))
    cond <- trimws(gsub("^\\-", "", cond))
    cond[nzchar(cond)]
  }
}


#' Reads log file from an ORS directory
#' @noRd
read_logs <- function(path) {
  log_path <- file.path(path, "logs", "ors.log")
  if (file.exists(log_path)) {
    readLines(log_path, encoding = "UTF-8")
  }
}


#' Checks if a process has failed
#' @noRd
assert_process <- function(proc) {
  status <- proc$status
  if (!is.na(status) && !identical(status, 0L)) {
    cli::cli_abort(
      c(
        "The docker command encountered an error",
        "Error code {proc$status}: {proc$stderr}"
      ),
      class = "ors_cmd_error"
    )
  }
}
