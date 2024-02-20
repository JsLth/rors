skip_if_offline("github.com")
skip_on_cran()
skip_on_ci()
skip_on_os(c("mac", "solaris"))
skip_if_not(docker_installed() && has_docker_access(), "docker unavailable")

ors <- local_ors_instance(
  dir = tempdir(),
  verbose = FALSE,
  dry = FALSE,
  version = "7c77ae5"
)

test_that("docker setup works", {
  # check if successful
  expect_true(ors$is_built())
  expect_true(ors$is_running())
  expect_true(ors$is_ready())

  # stop
  expect_no_error(ors$stop())
  expect_false(ors$is_running())

  # restart
  expect_no_error(ors$start())
  expect_output(ors$show_logs())

  # info
  expect_type(ors$get_container(), "list")
  expect_type(ors$get_image(), "list")
  expect_type(ors$get_status(), "list")

  # take down
  expect_no_error(ors$down())
  expect_false(ors$is_built())
})
