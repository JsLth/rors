test_that("Does ors_ready catch errors?", {
  expect_type(ready, "logical")
})

test_that("Test Docker availability", {
  ready <- ors_ready()
  skip_on_os(c("windows", "mac", "solaris"))
  skip_if_not_installed("sys")
  grant_docker_privileges(run = FALSE)
})

test_that("Test ORS Setup classes", {
  skip_if_not(ors_ready())
  ors <- expect_s3_class(ORSInstance$new(), "R6")
  expect_s3_class(ORSInstance$extract, "R6")
  expect_s3_class(ORSInstance$config(), "R6")
  expect_s3_class(ORSInstance$setup_settings(), "R6")
  expect_s3_class(ORSInstance$docker(), "R6")
  expect_equal(ors$dir, normalizePath("~", winslash = "/"))
  expect_true(ors$docker$docker_running)
  expect_type(ors$docker$image_built, "logical")
  expect_type(ors$docker$container_exists, "logical")
  expect_type(ors$docker$container_running, "logical")
  expect_false(ors$docker$service_ready)
})

test_that("Test ORSExtract", {
  skip_if_not_installed("osmextract")
  expect_type(ors$extract$get_extract("Cologne", "geofabrik"), "character")
  expect_type(ors$extract$path, "character")
  expect_true(file.exists(ors$path))
  expect_type(ors$extract$size, "double")
})


test_that("Test ORSConfig", {
  expect_type(ors$config$active_profiles, "list")
  expect_type(ors$config$ors_config, "list")
  ors$config$active_profiles <- "car"
  expect_equal(ors$config$ors_config$ors$services$routing$profiles$active, "car")
  expect_warning(ors$config$active_profiles <- "not-car")
  expect_equal(ors$config$open_config(), 0L)
})


test_that("Test ORSSetupSettings", {
  expect_type(ors$setup_settings$compose, "list")
  expect_type(ors$setup_settings$graph_building, "character")
  expect_type(ors$setup_settings$memory$total_memory, "double")
  expect_type(ors$setup_settings$memory$free_memory, "double")
  expect_equal(ors$setup_settings$open_compose(), 0L)
  expect_type(ors$setup_settings$memory$max_memory, "double")
  ors$setup_settings$allocate_memory()
  expect_equal(ors$setup_settings$memory$init_memory, ors$setup_settings$memory$max_memory / 2)
  check_gb <- function(gb) {
    if (is.na(gb)) {
      expect_equal(ors$setup_settings$compose$services$`ors-app`$environment[1], "BUILD_GRAPHS=False")
    } else if (identical(gb, "build")) {
      expect_named("build", ors$setup_settings$compose$services$`ors-app`)
    } else if (identical(gb, "change")) {
      expect_equal(ors$setup_settings$compose$services$`ors-app`$environment[1], "BUILD_GRAPHS=True")
      expect_type(ors$setup_settings$compose$services$`ors-app`$volumes[6], "character")
    }
  }
  gb <- ors$setup_settings$graph_building
  check_gb(gb)
  ors$setup_settings$graph_building <- "build"
  check_gb(gb)
  ors$setup_settings$graph_building <- "change"
  check_gb(gb)
  ors$setup_settings$graph_building <- NA
  check_gb(gb)
})