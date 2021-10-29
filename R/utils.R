# Title     : General utility functions
# Objective : Manipulate character strings and execute system commands
# Created by: Jonas Lieth
# Created on: 11.06.2021


#' @importFrom magrittr %>%
NULL


file.open <- function(file) {
  os <- .Platform$OS.type
  file <- shQuote(file)
  if (os == "unix") {
    system2("xdg-open", file, wait = FALSE)
  } else if (os == "windows") {
    system2("open", file, wait = FALSE)
  }
}


file_path_up <- function(path, times_back = NULL) {
  new_path <- normalizePath(path, winslash = "/") %>%
    strsplit("/") %>%
    unlist() %>%
    head(-times_back) %>%
    as.list() %>%
    do.call(file.path, .)
  return(new_path)
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


decode_non_base2 <- function(code) {
  base2_vector <- 0
  i <- 0
  while (tail(base2_vector, 1) < code) {
    base2_vector[i + 1] <- 2 ^ i
    i <- i + 1
  }
  base2_vector <- head(base2_vector, -1)
  base2_vector <- sort(base2_vector, decreasing = TRUE)
  for (b in seq(1, length(base2_vector))) {
    if (b > 1) {
      rbase2 <- tail(base2_vector, -(b - 1))
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


notify <- function(msg) {
  if (is.windows()) {
    system("rundll32 user32.dll, MessageBeep -1")
    system(sprintf("msg * \"%s\"", msg))

  } else if (is.linux()) {
    system("paplay /usr/share/sounds/freedesktop/stereo/complete.oga")
    system(paste(sprintf("notify-send \"%s\"", msg),
                 "\"Message from R\""))

  } else if (is.macos()) {

    system(paste("osascript -e 'display notification",
                 sprintf("\"%s\"", msg),
                 "with title",
                 "\"Message from R\""))
  }
  invisible()
}


is.rstudio <- function() {
  Sys.getenv("RSTUDIO") == 1
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
  inst <- system2(command = "which",
                  args = "docker",
                  stdout = FALSE,
                  stderr = FALSE)
  identical(inst, 0L)
}


#' Enable non-root docker access on Linux
#' @description Creates a docker group and adds the current user to it in order
#' to enable docker commands from within R. Doing this, either manually or by
#' using this function, is a requirement for using \code{\link{ORSInstance}}.
#' @param run If \code{FALSE}, the function will only return a logical vector
#' and will not change group memberships.
#' @details For details on what this function does and what security
#' implications it might have, refer to Docker's
#' \href{https://docs.docker.com/engine/install/linux-postinstall/}{post-installation guide}
#'
#' @export

grant_docker_privileges <- function(run = TRUE) {
  if (is.linux()) {
    is_granted <- function() {
      grepl("docker", system2("id", "-nG", stdout = TRUE))
    }

    if (!is_granted() && isTRUE(run)) {
      system2(command = "pkexec",
              args = paste("env DISPLAY=$DISPLAY XAUTHORITY=$XAUTHORITY bash -c",
                           "'sudo addgroup docker;",
                           "sudo usermod -aG docker $USER'"),
              stdout = "",
              stderr = "")
      sys::exec_internal("newgrp", "docker")
    }

    if (!is_granted()) {
      return(FALSE)
    }

    works <- identical(system2("docker", "ps"), 0L)
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
