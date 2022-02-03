# Title     : General utility functions
# Objective : Manipulate character strings and execute system commands
# Created by: Jonas Lieth
# Created on: 11.06.2021


file.open <- function(file) {
  file <- file
  if (is.linux()) {
    proc <- callr::process$new(command = "xdg-open", args = file)
  } else if (is.windows()) {
    proc <- callr::process$new(command = "open", args = file)
  }
  proc$get_exit_status()
}


file_path_up <- function(path, times_back = NULL) {
  path <- normalizePath(path, winslash = "/")
  new_path <- utils::head(unlist(strsplit(path, "/")), -times_back)
  do.call(file.path, as.list(new_path))
}


get_memory_info <- function() {
  if (is.windows()) {
    cmd <- paste(
      "(Get-WmiObject Win32_OperatingSystem)",
      "| %{'{{\"total\": {0},\n\"free\": {1}}}'",
      "-f $_.totalvisiblememorysize, $_.freephysicalmemory}"
    )
    
    mem_json <- callr::run("powershell", cmd, stdout = "|", stderr = NULL)
    parsed_json <- jsonlite::fromJSON(mem_json$stdout)
    lapply(parsed_json, function(x) as.numeric(x) / 1048576)
  } else if (is.linux()) {
    mem_csv <- callr::run("free", args = "--kibi", stdout = "|", stderr = NULL)
    mem_csv <- unlist(strsplit(mem_csv$stdout, "\n"))
    mem_csv <- paste(gsub("\\s+", ",", mem_csv), "\n")
    mem_df <- utils::read.csv(text = mem_csv)[1, c("total", "free")]
    lapply(mem_df, function(x) x / 1048576)
  }
}


relativePath <- function(targetdir, basedir = getwd()) {
  relative_path <- gsub(pattern = sprintf("%s|%s/", basedir, basedir),
                        replacement = "",
                        x = targetdir)
  if (relative_path == "") {
    relative_path <- "."
  }
  relative_path
}


capitalizeChar <- function(string) {
  cap_string <- tolower(as.character(string))
  substr(cap_string, 1, 1) <- toupper(substr(string, 1, 1))
  cap_string
}


base_profile <- function(profile) {
  strsplit(profile, "-")[[1]][1]
}


decode_base2 <- function(code) {
  is.base2 <- log2(code) %% 1 == 0 || code == 0
  if (isTRUE(is.base2)) {
    return(code)
  }
  base2_vector <- 0
  i <- 0
  while (utils::tail(base2_vector, 1) < code) {
    base2_vector[i + 1] <- 2 ^ i
    i <- i + 1
  }
  base2_vector <- rev(utils::head(base2_vector, -1))
  for (b in seq(1, length(base2_vector))) {
    if (b > 1) {
      rbase2 <- utils::tail(base2_vector, -(b - 1))
    } else rbase2 <- base2_vector
    res <- NULL
    for (ni in seq(1, length(rbase2))) {
      num <- rbase2[ni]
      code_sum <- num + sum(res[!is.na(res)])
      if (code_sum <= code) {
        res[ni] <- num
      }
      if (code_sum == code) {
        return(res[!is.na(res)])
      }
    }
  }
}


box <- function(x) {
  if (length(x) == 1) {
    list(x)
  } else x
}


df_nest <- function(data, names = NULL) {
  rows <- unique(sapply(data, nrow))
  if (length(rows) > 1) {
    cli::cli_abort("Data have different number of rows.")
  }

  if (!is.null(names)) {
    names(data) <- names
  }
  data
}


notify <- function(msg) {
  if (interactive()) {
    if (is.windows()) {
      cmd <- paste(
        "[reflection.assembly]::loadwithpartialname(\"System.Windows.Forms\");",
        "[reflection.assembly]::loadwithpartialname(\"System.Drawing\");",
        "$notify = new-object system.windows.forms.notifyicon;",
        "$notify.icon = [System.Drawing.SystemIcons]::Information;",
        "$notify.visible = $true;",
        "$notify.showballoontip(10,",
        "\"Message from R\",",
        sprintf("\"%s\",", msg),
        "[system.windows.forms.tooltipicon]::None)"
      )
      callr::run("powershell", cmd, stdout = NULL, error_on_status = FALSE)
    } else if (is.linux()) {
      cmd1 <- "/usr/share/sounds/freedesktop/stereo/complete.oga"
      cmd2 <- "%s \"Message from R\""
      callr::run("paplay", cmd1, stdout = NULL, error_on_status = FALSE)
      callr::run("notify-send", cmd2, stdout = NULL, error_on_status = FALSE)
    } else if (is.macos()) {
      cmd <- paste(
        "-e 'display notification",
        sprintf("\"%s\"", msg),
        "with title",
        "\"Message from R\"")
      callr::run("osascript", cmd, stdout = NULL, error_on_status = FALSE)
    }
    invisible()
  }
}


is.rstudio <- function() {
  .Platform$GUI == "RStudio"
}


is.windows <- function() {
  .Platform$OS.type == "windows"
}


is.linux <- function() {
  Sys.info()["sysname"] == "Linux"
}


is.macos <- function() {
  Sys.info()["sysname"] == "Darwin"
}


isTRUEorFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}


docker_installed <- function() {
  inst <- callr::run(
    command = "which",
    args = "docker",
    stdout = NULL,
    stderr = NULL
  )
  identical(inst$status, 0L)
}


#' Enable non-root docker access on Linux
#' @description Creates a docker group and adds the current user to it in order
#' to enable docker commands from within R. Doing this, either manually or by
#' using this function, is a requirement for using \code{\link{ORSInstance}}.
#' @param run If \code{FALSE}, the function will only return a logical vector
#' and will not change group membership.
#' @details For details on what this function does and what security
#' implications it might have, refer to Docker's
#' \href{https://docs.docker.com/engine/install/linux-postinstall/}{post-installation guide}
#'
#' @export

grant_docker_privileges <- function(run = TRUE) {
  if (is.linux()) {
    is_granted <- function() {
      grepl("docker", callr::run("id", args = "-nG", stdout = "|")$stdout)
    }

    if (!is_granted() && isTRUE(run)) {
      system2(command = "pkexec",
              args = paste("env DISPLAY=$DISPLAY XAUTHORITY=$XAUTHORITY bash -c",
                           "'sudo addgroup docker;",
                           "sudo usermod -aG docker $USER'"),
              stdout = "",
              stderr = "")
      callr::run("newgrp", args = "docker", stdout = NULL, stderr = NULL)
    }

    if (!is_granted()) {
      return(FALSE)
    }

    works <- callr::run("docker", "ps", stdout = FALSE, stderr = FALSE) == 0L
    if (!works) {
      cli::cli_alert_warning(paste("You might have to restart your system to",
                                   "re-evaluate group membership"))
    }

    return(TRUE)
  } else TRUE
}


cli_abortifnot <- function(expr) {
  if (isFALSE(expr)) {
    uneval_expr <- deparse(substitute(expr))
    cli::cli_abort("{.code {uneval_expr}} is {.val {FALSE}}.",
                   call = sys.call(-1))
  }
}
