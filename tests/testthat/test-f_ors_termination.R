test_that("Test ORS wipe", {
  if (!dir.exists(temp_dir <- "fixtures")) {
    temp_dir <- dir.create("fixtures", showWarnings = TRUE)
  }

  ors <- ORSInstance$new(dir = "fixtures")
  dir <- ors$dir

  ors$docker$cleanup()
  expect_false(dir.exists(file.path(dir, "docker/conf")))
  expect_false(dir.exists(file.path(dir, "docker/graphs")))
  expect_false(dir.exists(file.path(dir, "docker/elevation_cache")))

  ors$docker$error_log <- NULL
  expect_false(dir.exists(file.path(dir, "docker/logs")))

  ors$remove(ignore_image = TRUE)
  expect_false(dir.exists(dir))
  expect_false(ors$active)
  expect_equal(suppressWarnings(ors$extract), NULL)
  expect_equal(suppressWarnings(ors$config), NULL)
  expect_equal(suppressWarnings(ors$setup_settings), NULL)
  expect_equal(suppressWarnings(ors$docker), NULL)
  expect_warning(ors$extract, "not active")
  expect_warning(ors$initial_setup(), "not active")
  expect_warning(ors$remove(), "not active")
  unlink("fixtures", recursive = TRUE)
})