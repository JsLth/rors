load("fixtures/setupdone")
unlink("fixtures/setupdone")

test_that("Test docker termination", {
  skip_if_not(setup_done)
  expect_container_down(ors$docker)
  expect_image_removed(ors$docker)
})

test_that("Test directory wipe", {
  skip("Skip until ors name customization is implemented.")
  dir <- ors$dir
  skip_if(!dir.exists(ors$dir))

  ors$docker$cleanup()
  expect_false(dir.exists(file.path(dir, "docker/data")))
  expect_false(dir.exists(file.path(dir, "docker/graphs")))
  expect_false(dir.exists(file.path(dir, "docker/elevation_cache")))

  ors$error_log <- NULL
  expect_false(dir.exists(file.path(dir, "docker/logs")))

  ors$remove()
  expect_false(dir.exists(dir))
  expect_false(exists(ors))
})