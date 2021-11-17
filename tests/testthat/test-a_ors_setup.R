ready <- ors_ready()

test_that("Does ors_ready catch errors?", {
  expect_type(ready, "logical")
})

skip_if(ready, "ORS is already set up. Skipping setup tests.")

test_that("Test Docker availability", {
  skip_on_os(c("windows", "mac", "solaris"))
  skip_if_not_installed("sys")
  privileged <- grant_docker_privileges(run = FALSE)
  expect_type(privileged, "logical")
})

skip_if(Sys.info()["sysname"] == "Linux" && isFALSE(privileged))
skip_if_offline("github.com")
ors <- ORSInstance$new()

test_that("Test ORS Setup classes", {
  expect_s3_class(ors, "R6")
  expect_s3_class(ors$extract, "R6")
  expect_s3_class(ors$config, "R6")
  expect_s3_class(ors$setup_settings, "R6")
  expect_s3_class(ors$docker, "R6")
  expect_equal(ors$dir, normalizePath("~/openrouteservice-master", winslash = "/"))
})

test_that("Test ORSExtract", {
  extract <- ors$extract
  skip_if_not_installed("osmextract")
  expect_type(extract$get_extract("Cologne", "geofabrik"), "character")
  expect_type(extract$path, "character")
  expect_true(file.exists(extract$path))
  expect_type(extract$size, "double")
})


test_that("Test ORSConfig", {
  config <- ors$config
  expect_type(config$active_profiles, "list")
  expect_type(config$ors_config, "list")
  config$active_profiles <- "car"
  expect_equal(config$ors_config$ors$services$routing$profiles$active, list("car"))
  expect_warning(config$active_profiles <- "not-car")
})

test_that("Test ORSSetupSettings", {
  setup_settings <- ors$setup_settings
  expect_type(setup_settings$compose, "list")
  expect_type(setup_settings$memory$total_memory, "double")
  expect_type(setup_settings$memory$free_memory, "double")
  setup_settings$allocate_memory()
  expect_type(setup_settings$memory$max_memory, "double")
  expect_type(setup_settings$memory$init_memory, "double")
  expect_equal(setup_settings$memory$init_memory, setup_settings$memory$max_memory / 2)
  check_gb <- function() {
    gb <- setup_settings$graph_building
    if (is.na(gb)) {
      expect_type(setup_settings$compose$services$`ors-app`$environment[1], "character")
    } else if (identical(gb, "build")) {
      expect_element(names(setup_settings$compose$services$`ors-app`), "build")
    } else if (identical(gb, "change")) {
      expect_equal(setup_settings$compose$services$`ors-app`$environment[1], "BUILD_GRAPHS=True")
      expect_type(setup_settings$compose$services$`ors-app`$volumes[6], "character")
    }
  }
  check_gb()
  setup_settings$graph_building <- "build"
  check_gb()
  setup_settings$graph_building <- "change"
  check_gb()
  setup_settings$graph_building <- NA
  check_gb()
})

test_that("Test ORSDockerInterface", {
  docker <- ors$docker
  expect_true(docker$docker_running)
  expect_type(docker$image_built, "logical")
  expect_type(docker$container_exists, "logical")
  expect_type(docker$container_running, "logical")
  expect_type(docker$service_ready, "logical")
  expect_type(docker$error_log, "NULL")
})

setup_done <- test_that("Test container setup", {
  skip("Skip until it is sorted out how to properly implement this.")
  # These functions take long and can change docker setups and thus should not
  # be run automatically
  skip_if_not_explicit()
  expect_container_start(ors$docker)
  expect_container_stop(ors$docker)
  expect_container_build(ors$docker)
})
print(getwd())
save(setup_done, file = "fixtures/setupdone")