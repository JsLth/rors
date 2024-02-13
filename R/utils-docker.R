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
