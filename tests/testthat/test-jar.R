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
  print(1)
  expect_false(ors$is_init())
  print(2)
  expect_false(ors$is_running())
  print(3)
  expect_false(ors$is_ready())
  print(4)
  expect_true(ors$is_mounted())
  print(5)

  # check if service is startable
  ors$set_extract(file = system.file("setup/monaco.pbf", package = "rors"))
  print(6)
  ors$set_port()
  print(7)
  ors$up()
  print(8)

  # service is started - should return true
  expect_true(ors$is_init())
  print(9)
  expect_true(ors$is_running())
  print(10)
  expect_true(ors$is_ready())
  print(11)

  # stop
  ors$stop()
  print(12)
  expect_false(ors$is_running())
  expect_true(ors$is_init())
})
