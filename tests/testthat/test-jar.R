message("jar")

test_that("versions are caught correctly", {
  with_mocked_bindings(
    run = function(...) list(stderr = "java version \"22\" 2024-03-19"),
    .package = "callr",
    expect_no_error(check_jdk_version(FALSE))
  )

  with_mocked_bindings(
    run = function(...) list(stderr = "java 22.0.2 2024-03-19"),
    .package = "callr",
    expect_no_error(check_jdk_version(FALSE))
  )

  with_mocked_bindings(
    run = function(...) list(stderr = "openjdk 22.0.2 2024-07-16"),
    .package = "callr",
    expect_no_error(check_jdk_version(FALSE))
  )

  with_mocked_bindings(
    run = function(...) list(stderr = "openjdk version 22 2024-07-16"),
    .package = "callr",
    expect_no_error(check_jdk_version(FALSE))
  )

  with_mocked_bindings(
    run = function(...) list(stderr = "openjdk 11.0.24 2024-07-16"),
    .package = "callr",
    expect_error(check_jdk_version(FALSE), class = "ors_java_version_error")
  )
})

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

  # both types of logs should return something
  expect_gt(length(ors$show_logs(source = "logfile")), 0)
  expect_gt(length(ors$show_logs(source = "process")), 0)

  # service is started - should return true
  expect_true(ors$is_init())
  expect_true(ors$is_running())
  expect_true(ors$is_ready())

  # stop
  ors$stop()
  expect_false(ors$is_running())
  expect_true(ors$is_init())
})

rors_cleanup()
