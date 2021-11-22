# Skips -----------------------------------------------------------------------

skip_if_on_ci <- function() {
  on_ci <- as.logical(Sys.getenv("GITLAB_CI"))
  testthat::skip_if(isTRUE(on_ci), message = "On GitLab CI")
}

skip_if_not_explicit <- function() {
  testthat::skip_on_appveyor()
  testthat::skip_on_bioc()
  testthat::skip_on_covr()
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  testthat::skip_on_ci()
  testthat::skip_if_not(interactive(), "Test call not explicit")
}


skip_if_ors_not_ready <- function() {
  if (isFALSE(ors_ready(force = TRUE))) {
    testthat::skip("ORS service is not reachable.")
  }
}


# Expects ---------------------------------------------------------------------

expect_element <- function(object, element) {
  object_collapsed <- paste(object, collapse = ", ")

  testthat::expect(
    is.element(element, object),
    sprintf("%s is not an element of %s", element, object_collapsed)
  )

  invisible(object)
}


expect_container_start <- function(object) {
  if (object$container_running) {
    object$stop_container()
  }

  object$start_container(wait = TRUE)

  testthat::expect(
    isTRUE(object$container_running && object$service_ready),
    "$start_container did not start the container."
  )

  invisible(object)
}


expect_container_stop <- function(object) {
  if (!object$container_running) {
    warning("Container is not running.")
    object$start_container(wait = TRUE)
  }

  object$stop_container()

  testthat::expect(
    isFALSE(object$container_running && object$service_ready),
    "$stop_container() did not stop the container."
  )

  invisible(object)
}


expect_container_build <- function(object, init_setup = TRUE) {
  if (object$container_running) {
    warning("Container already running.")
    object$stop_container()
  }

  if (object$container_exists) {
    warning("Container already built.")
    object$container_down()
  }

  if (object$image_built) {
    warning("Image already built")
  }

  if (!init_setup) {
    object$image_up(wait = TRUE, verbose = FALSE)
  } else {
    object$initial_setup()
  }

  testthat::expect(
    isTRUE(object$image_built && object$container_exists && object$container_running),
    "$image_up() did not build the image or the container."
  )

  invisible(object)
}


expect_container_down <- function(object) {
  if (!object$container_exists) {
    warning("Container is already down.")
    invisible()
  }

  object$container_down()

  testthat::expect(
    isFALSE(object$image_built && object$container_exists && object$container_running),
    "$container_down() did not take down the container"
  )

  invisible(object)
}


expect_image_removed <- function(object) {
  if (!object$image_built) {
    warning("Image does not exist.")
    invisible()
  }

  if (object$container_running) {
    warning("Container is still running.")
    invisible()
  }

  object$rm_image()

  testthat::expect(
    isFALSE(object$image_built && object$container_exists),
    "$rm_image() did not remove the image."
  )

  invisible(object)
}


# Fixture helpers -------------------------------------------------------------

