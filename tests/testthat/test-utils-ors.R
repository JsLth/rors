skip_if(!loadable("webfakes"), "webfakes unavailable")
skip_webfakes <- function(web) {
  failed <- inherits(web, "try-error")
  skip_if(failed, "webfakes bug: https://github.com/r-lib/webfakes/issues/103")
}

test_that("ors_ready() works", {
  server <- "pub"
  app <- webfakes::new_app()
  app$get("/ors/v2/health", function(req, res) {
    counter <- res$app$locals$counter
    if (is.null(counter)) counter <- 0
    ready <- ifelse(counter == 0, "ready", "not ready")
    res$app$locals$counter <- counter + 1
    res$send_json(list(status = ready), auto_unbox = TRUE)
  })
  web <- try(webfakes::local_app_process(app, start = TRUE))
  skip_webfakes(web)
  ors <- ors_instance(server = web$url())

  expect_true(ors_ready())
  expect_true(ors_ready(force = FALSE))
  expect_error(ors_ready(error = TRUE))
  expect_false(ors_ready())
})


test_that("ors_status() formats correctly", {
  ors_instance(server = "public")
  expect_type(ors_status(), "list")
  expect_identical(
    unclass(get_profiles()),
    unlist(base_profiles(), use.names = FALSE)
  )

  app <- webfakes::new_app()
  app$get("/ors/v2/health", function(req, res) res$send_json(list(status = "ready")))
  app$get("/ors/v2/status", function(req, res) {
    path <- test_path("fixtures/status.json")
    status <- jsonlite::read_json(path, simplifyVector = TRUE)
    res$send_json(status)
  })
  web <- try(webfakes::local_app_process(app, start = TRUE))
  skip_webfakes(web)

  ors <- ors_instance(server = web$url())

  status <- ors_status()
  expect_s3_class(status, "ors_status")
  expect_type(status, "list")
  status_mod <- status
  status_mod$languages <- paste(status$languages, collapse = ", ")
  prnt <- capture.output(print(status))
  expect_identical(unclass(status_mod), yaml::yaml.load(prnt))
  expect_identical(get_profiles(), "driving-car")
})


test_that("cache recovery works", {
  assign("test", "successful", envir = ors_cache)
  test_recover <- function(force = FALSE) {
    recover_from_cache(test, force = force)
    "not successful"
  }
  expect_identical(test_recover(), "successful")
  expect_identical(test_recover(force = TRUE), "not successful")
})


test_that("logs are split correctly", {
  logs <- readChar(test_path("fixtures/testlog.txt"), 10012)
  expect_length(split_by_log_entry(logs), 77)
})
