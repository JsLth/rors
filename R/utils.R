#' Opens a file
#' @returns Exit status
#' @noRd
file.open <- function(file) {
  file <- file
  if (is_linux()) {
    proc <- callr::process$new(command = "xdg-open", args = file)
  } else if (is_windows()) {
    proc <- callr::process$new(command = "open", args = file)
  }
  proc$get_exit_status()
}


#' Given a path, returns the parent path
#' @param times_back Number of times to up a folder
#' @noRd
file_path_up <- function(path, times_back = 1L) {
  path <- normalizePath(path, winslash = "/")
  new_path <- utils::head(unlist(strsplit(path, "/")), -times_back)
  do.call(file.path, as.list(new_path))
}


#' Checks if a URL is local (i.e. localhost or 127.0.0.1)
#' @noRd
is_local <- function(url) {
  grepl("^(https?:\\/\\/)?[[:alnum:]\\.]+\\:[[:digit:]]+\\/?", url, perl = TRUE)
}


#' Checks if a character string is a valid URL
#' @noRd
is_url <- function(url) {
  grepl("^(https?:\\/\\/)?[[:alnum:]\\.]+\\.[[:lower:]]+\\/?", url, perl = TRUE)
}


#' Checks if a URL leads to the public API of OpenRouteService (api.openrouteservice)
#' @noRd
is_ors_api <- function(url) {
  grepl("api.openrouteservice.org", url, fixed = TRUE)
}


#' For Windows and Linux, returns the total and available memory of the system
#' @returns List containing total and available memory
#' @noRd
get_memory_info <- function() {
  if (is_windows()) {
    cmd <- paste(
      "(Get-WmiObject Win32_OperatingSystem)",
      "| %{'{{\"total\": {0},\n\"free\": {1}}}'",
      "-f $_.totalvisiblememorysize, $_.freephysicalmemory}"
    )

    mem_json <- callr::run("powershell", cmd, stdout = "|", stderr = NULL)
    parsed_json <- jsonlite::fromJSON(mem_json$stdout)
    lapply(parsed_json, function(x) as.numeric(x) / 1048576L)
  } else if (is_linux()) {
    mem_csv <- callr::run("free", args = "--kibi", stdout = "|", stderr = NULL)
    mem_csv <- unlist(strsplit(mem_csv$stdout, "\n"))
    mem_csv <- paste(gsub("\\s+", ",", mem_csv), "\n")
    mem_df <- utils::read.csv(text = mem_csv)[1, c("total", "free")]
    lapply(mem_df, function(x) x / 1048576L)
  }
}


#' Given two absolute paths, constructs a relative path
#' @param targetdir Path that should be turned to a relative path
#' @param basedir Path that contains `targetdir`
#' @param pretty Whether to add a tilde and a slash in front
#' @noRd
relative_path <- function(targetdir, basedir = getwd(), pretty = FALSE) {
  relative_path <- gsub(
    pattern = sprintf("%s|%s/", basedir, basedir),
    replacement = "",
    x = targetdir
  )
  if (relative_path == "") {
    relative_path <- "."
  }

  if (pretty) {
    relative_path <- paste0("~/", relative_path)
  }

  relative_path
}


#' Capitalizes the first symbol of a character string
#' @noRd
capitalize_char <- function(string) {
  cap_string <- tolower(as.character(string))
  substr(cap_string, 1L, 1L) <- toupper(substr(string, 1L, 1L))
  cap_string
}


#' Given an ORS profile name, returns the first part of the profile, e.g.,
#' driving-car -> driving
#' @noRd
base_profile <- function(profile) {
  if (!is.na(profile)) {
    strsplit(profile, "-")[[1L]][1L]
  }
}


#' Simple wrapper around regexec and regmatches to find a regex pattern in a text
#' @noRd
regex_match <- function(text, pattern, ...) {
  regmatches(text, regexec(pattern, text, ...))
}


count <- function(x) {
  df <- aggregate(x, list(x), length)
  names(df) <- c("level", "count")
  df[order(df$count, decreasing = TRUE), ]
}


#' Binds list of (sf) data.frames to a single data.frame. If the number of
#' columns differs, fills empty columns with NA
#' @param args List of data.frames or sf objects
#' @returns data.frame or sf data.frame
#' @noRd
rbind_list <- function(args) {
  nam <- lapply(args, names)
  unam <- unique(unlist(nam))
  len <- vapply(args, length, numeric(1))
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    if (!nrow(args[[i]])) {
      for (n in unam) `$<-`(args[[i]], n, character())
    } else {
      nam_diff <- setdiff(unam, nam[[i]])
      if (length(nam_diff)) {
        args[[i]][setdiff(unam, nam[[i]])] <- NA
      }
    }
  }
  out <- do.call(rbind, args)
  row.names(out) <- NULL
  out
}


#' Converts a decimal to its binary representation, e.g. 3 -> c(2, 1)
#' @returns Numeric vector of varying length
#' @noRd
decode_base2 <- function(code) {
  is_base2 <- log2(code) %% 1L == 0L || code == 0L
  if (is_base2) {
    return(code)
  }
  base2_vector <- 0L
  i <- 0L
  while (utils::tail(base2_vector, 1L) < code) {
    base2_vector[i + 1L] <- 2L^i
    i <- i + 1L
  }
  base2_vector <- rev(utils::head(base2_vector, -1L))
  for (b in seq(1L, length(base2_vector))) {
    if (b > 1L) {
      rbase2 <- utils::tail(base2_vector, -(b - 1L))
    } else {
      rbase2 <- base2_vector
    }
    res <- NULL
    for (ni in seq(1L, length(rbase2))) {
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


#' Turns a length-1 vector to a single-element list
#' @noRd
box <- function(x) {
  if (length(x) == 1L) {
    list(x)
  } else {
    x
  }
}


#' Turns input data.frames to a single data.frame without seperating the columns.
#' Comparable to tidyr::nest, just with base R
#' @param ... Data.frames where the argument name is the column name of the
#' entire data.frame within the output data.frame and the names are the column
#' names within the data.frame that should be nested.
#' @returns Nested data.frame where each nested data.frame can be accessed using
#' their argument in `...`.
#' @noRd
df_nest <- function(...) {
  data <- list(...)
  structure(
    data,
    row.names = seq_len(nrow(data[[1]])),
    class = "data.frame"
  )
}


#' Makes a subtle system sound and displays a notification window
#' @param msg Message to be displayed.
#' @noRd
notify <- function(msg) {
  if (interactive()) {
    if (is_windows()) {
      if (!identical(system2("powershell", stdout = FALSE), 127L)) {
        cmd <- paste(
          "[System.Reflection.Assembly]::LoadWithPartialName(\"System.Windows.Forms\");",
          "$notify = New-Object System.Windows.Forms.Notifyicon;",
          "$notify.Icon = [System.Drawing.SystemIcons]::Information;",
          "$notify.BalloonTipIcon = \"Info\";",
          sprintf("$notify.BalloonTipText = \"%s\";", msg),
          "$notify.BalloonTipTitle = \"Message from R\";",
          "$notify.Visible = $True;",
          "$notify.ShowBalloonTip(10)"
        )
        callr::run("powershell", cmd, stdout = NULL, stderr = NULL, error_on_status = FALSE)
      } else {
        system("rundll32 user32.dll, MessageBeep -1")
      }
    } else if (is_linux()) {
      cmd1 <- "/usr/share/sounds/freedesktop/stereo/complete.oga"
      cmd2 <- c("Message from R", msg)
      callr::run("paplay", cmd1, stdout = NULL, stderr = NULL, error_on_status = FALSE)
      callr::run("notify-send", cmd2, stdout = NULL, stderr = NULL, error_on_status = FALSE)
    } else if (is_macos()) {
      cmd <- paste(
        "-e 'display notification",
        sprintf("\"%s\"", msg),
        "with title",
        "\"Message from R\""
      )
      callr::run("osascript", cmd, stdout = NULL, stderr = NULL, error_on_status = FALSE)
    }
    invisible()
  }
}


#' Checks if Windows is the current OS
#' @noRd
is_windows <- function() {
  .Platform$OS.type == "windows"
}


#' Checks if Linux is the current OS
#' @noRd
is_linux <- function() {
  Sys.info()["sysname"] == "Linux"
}


#' Checks if Mac OS is the current OS
#' @noRd
is_macos <- function() {
  Sys.info()["sysname"] == "Darwin"
}


#' Checks if Docker is installed on the system.
#' @noRd
docker_installed <- function() {
  docker_path <- Sys.which("docker")
  installed <- any(as.logical(nchar(docker_path)))
  installed
}


#' Checks if Docker is reachable and running
#' @noRd
docker_running <- function() {
  callr::run(
    "docker",
    "ps",
    stdout = NULL,
    stderr = NULL,
    error_on_status = FALSE
  )$status == 0L
}


check_docker_installation <- function() {
  if (!docker_installed()) {
    link <- cli::style_hyperlink(
      text = "Docker",
      link = "https://docs.docker.com/get-docker/"
    )
    cli::cli_abort(c(
      "!" = "No Docker installation could be detected.",
      "i" = paste(
        "A", link, "installation is needed before setting up a local ORS",
        "instance"
      )
    ))
  }
}



#' Checks if current user can access docker, i.e. if user is included in
#' the docker group
#' @noRd
has_docker_access <- function() {
  if (is_linux()) {
    grepl("docker", callr::run("id", args = "-nG", stdout = "|")$stdout)
  }
}


#' Checks if current user is root
#' @noRd
is_root <- function() {
  if (is_linux()) {
    grepl("root", callr::run("whoami")$stdout)
  }
}


#' Checks if Docker can be accessed and gives an informative error if not
#' @noRd
check_docker_access <- function() {
  if (!is_root() && !has_docker_access() && !docker_running()) {
    link <- cli::style_hyperlink(
      text = "Linux post-installation guide",
      url = "https://docs.docker.com/engine/install/linux-postinstall/"
    )
    cli::cli_abort(c(
      "!" = "Cannot access Docker as a non-root user: permission denied.",
      "i" = paste(
        "You may want to run R as root or follow Docker's", link, "to set up",
        "Docker access for non-root users."
      )
    ))
  }
}


#' stopifnot but with the cli package
#' @noRd
cli_abortifnot <- function(expr) {
  if (isFALSE(expr)) {
    uneval_expr <- deparse(substitute(expr))
    cli::cli_abort("{.code {uneval_expr}} is {.val {FALSE}}.",
      call = sys.call(-1L)
    )
  }
}


slow_edit <- function(file, ...) {
  if (!interactive()) {
    cli::cli_warn("Cannot edit a file interactively in batch mode.")
  }

  cli::cli_process_start(
    "Editing file...",
    msg_done = "Manual editing complete.",
    msg_failed = "Cannot edit file."
  )

  if (!is.null(get0("RStudio.Version"))) {
    open_file <- get0(".rs.api.documentOpen")
    get_context <- get0(".rs.api.getSourceEditorContext")
    if (any(sapply(c(open_file, get_context), is.null))) {
      cli::cli_abort("Cannot retrieve RStudio specific editor functions.")
    }
    open_file(file)
    path <- .rs.api.getSourceEditorContext()$path
    while (.rs.api.getSourceEditorContext()$path == path) {
      Sys.sleep(0.3)
    }
  } else {
    if (!is_windows()) {
      cli::cli_abort("Interactive editing in RGUI is only supported for windows.")
    }
    r_handles <- getWindowsHandles()
    file.edit(file, ...)
    r_handles2 <- getWindowsHandles()
    editor <- r_handles2[!r_handles2 %in% r_handles]
    while (all(editor %in% getWindowsHandles(minimized = TRUE))) {
      Sys.sleep(0.3)
    }
  }

  cli::cli_process_done()
}
