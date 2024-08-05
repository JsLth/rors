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
    cond_fmt <- cli::cli_vec(cond$error, style = list(vec_sep = "\n\n"))
    msg <- c("The service ran into the following errors:", cond_fmt)
    cli::cli_abort(msg, call = NULL, class = "ors_setup_error")
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
    jar = read_logfile(src, last = 1)
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
    cond <- gsub("\r", "", cond)
    cond[nzchar(cond)]
  }
}


#' Reads log file from an ORS directory
#' @param path Path to the top directory
#' @param last Last n application starts to return
#' @noRd
read_logfile <- function(path, last = NULL) {
  log_path <- file.path(path, "logs", "ors.log")
  if (!file.exists(log_path)) {
    return(NULL)
  }

  # split log entries not by line but by timestamp
  # this is useful to capture entire error or warning messages
  logs <- readChar(log_path, nchars = file.info(log_path)$size)
  rgx <- "(?<=\n)(?=[0-9]{4}-[0-9]{1,2}-[0-9]{1,2})"
  logs <- strsplit(logs, rgx, perl = TRUE)[[1]]

  if (!is.null(last)) {
    # an application start occurs when the log signals "Starting Application"
    idx <- last(grep("Starting Application", logs, fixed = TRUE))
    logs <- logs[seq(idx, length(logs))]
  }

  logs
}


#' Checks if a process has failed
#' @noRd
assert_process <- function(proc) {
  status <- proc$status
  if (!is.na(status) && !identical(status, 0L)) {
    msg <- c(
      "The docker command encountered an error",
      "Error code {proc$status}: {proc$stderr}"
    )
    abort(msg, class = "cmd_error")
  }
}
