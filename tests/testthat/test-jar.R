test_that("properly stops without java", {
  expect_error(with_mocked_bindings(
    has_util = function(...) FALSE,
    get_java_version(FALSE)
  ), class = "ors_java_missing_error")
})

skip_on_cran()
skip_if_not(has_valid_java(), "java unavailable")
skip_if_offline("github.com")

ors <- local_ors_instance(type = "jar", verbose = FALSE)

test_that("jar setup works", {
  # service not started - should return false
  expect_false(ors$is_init())
  expect_false(ors$is_running())
  expect_false(ors$is_ready())
  expect_true(ors$is_mounted())

  # check if service is startable
  ors$set_extract(file = system.file("setup/monaco.pbf", package = "rors"))
  ors$set_port()
  ors$up()

  # service is started - should return true
  expect_true(ors$is_init())
  expect_true(ors$is_running())
  expect_true(ors$is_ready())

  # stop
  ors$stop()
  expect_false(ors$is_running())
  expect_true(ors$is_init())
})
