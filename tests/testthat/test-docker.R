skip_if_offline("github.com")
skip_on_cran()
if (on_os("darwin")) {
  skip_on_ci()
}
skip_on_os("solaris")
skip_if_not(docker_installed() && has_docker_access(), "docker unavailable")

ors <- local_ors_instance(verbose = FALSE, dry = FALSE, version = "8.0.0")
ors$set_extract(file = test_pbf())
withr::local_options(list(rors_setup_warn = FALSE))

skip_if_not(startsWith(ors$compose$name, "test-rors"))

test_that("build checks can say no", {
  expect_false(ors$is_built())
  expect_false(ors$is_running())
  expect_false(ors$is_ready())
})

test_that("ors setup works with non-default extract", {
  ors$up()
  expect_true(ors$is_built())
  expect_true(ors$is_running())
  expect_true(ors$is_ready())
  expect_no_error(ors_inspect(test_coords(1:2), as = "list"))
})

test_that("can stop", {
  expect_no_error(ors$stop())
  expect_false(ors$is_running())
  expect_false(ors$is_ready())
})

test_that("can restart", {
  expect_no_error(ors$start())
})

test_that("logs are formatted", {
  logs <- ors$show_logs()
  expect_type(logs, "character")
  expect_gt(length(logs), 10) # arbitrary length that is higher than a few
  expect_true(any(cli::ansi_has_any(logs)))
  expect_false(any(cli::ansi_has_any(ors$show_logs(format = FALSE))))
})

test_that("info methods work", {
  expect_type(ors$get_container(), "list")
  expect_type(ors$get_image(), "list")
  expect_type(ors$get_status(), "list")
})

test_that("can die", {
  expect_no_error(ors$down())
  expect_false(ors$is_built())
})
