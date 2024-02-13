skip_if_docker_unavailable <- function() {
  skip_if_not(docker_installed() && has_docker_access(), "docker unavailable")
}

is_mock_test <- function() {
  !nzchar(Sys.getenv("REAL_REQUESTS"))
}
