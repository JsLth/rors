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
  proc$get_exit_status
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
  } else if (is_macos()) {
    cmd <- c("-l 1", "-s 0", "| grep PhysMem")
    mem <- callr::run("top", cmd, stdout = "|", stderr = NULL)$stdout
    used <- as.numeric(regex_match(mem, "([0-9]+)M used"))[[1]][2] / 1024
    free <- as.numeric(regex_match(mem, "([0-9]+)M unused"))[[1]][2] / 1024
    list(total = used + free, free = free)
  }
}


#' Makes a subtle system sound and displays a notification window
#' @param msg Message to be displayed.
#' @noRd
notify <- function(msg) {
  if (!interactive()) return(invisible())
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


#' Checks if current user is root
#' @noRd
is_root <- function() {
  if (is_linux()) {
    grepl("root", callr::run("whoami")$stdout)
  } else {
    TRUE
  }
}
