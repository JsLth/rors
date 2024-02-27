#' Retrieves information about the image of a container
#' @param name Container name
#' @noRd
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


#' Retrieves information about a container
#' @param name Container name
#' @noRd
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


#' Retrieves logs for a container
#' @param name Name of the container
#' @noRd
docker_logs <- function(name) {
  cmd <- c("logs", name)
  logs <- callr::run(
    "docker",
    args = cmd,
    stdout = "|",
    stderr = "2>&1",
    error_on_status = FALSE,
    encoding = "UTF-8"
  )

  if (identical(logs$status, 0L)) {
    strsplit(logs$stdout, "\n")[[1]]
  }
}


#' Checks if a container is built
#' @param name Container name
#' @noRd
container_built <- function(name) {
  if (!docker_running()) return(NA)
  cmd <- c("ps", "-a", "--format", "{{ .Names }}")

  container_names <- callr::run(
    "docker",
    args = cmd,
    stdout = "|",
    stderr = "|",
    error_on_status = TRUE
  )

  name %in% strsplit(container_names$stdout, "\n")[[1]]
}


#' Checks if a container is running
#' @param name Container name
#' @noRd
container_running <- function(name) {
  if (!docker_running()) return(FALSE)
  cmd <- c(
    "container", "ls", "-a", "--format", "\"{{.State}}\"",
    "--filter", sprintf("name=^/%s$", name)
  )

  container_check <- callr::run(
    command = "docker",
    args = cmd,
    stdout = "|",
    stderr = "|",
    error_on_status = TRUE
  )

  grepl("running", container_check$stdout, fixed = TRUE)
}


#' Checks if Docker is installed on the system.
#' @noRd
docker_installed <- function() {
  any(as.logical(nchar(Sys.which("docker"))))
}


#' Checks if current user can access docker, i.e. if user is included in
#' the docker group
#' @noRd
has_docker_access <- function() {
  if (is_linux()) {
    grepl("docker", callr::run("id", args = "-nG", stdout = "|")$stdout)
  } else {
    TRUE
  }
}


#' Checks if Docker is reachable and running
#' @noRd
docker_running <- function() {
  if (!docker_installed()) return(FALSE)
  callr::run(
    "docker",
    "ps",
    stdout = NULL,
    stderr = NULL,
    error_on_status = FALSE
  )$status == 0L
}


#' Verifies that docker is installed
#' @noRd
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
