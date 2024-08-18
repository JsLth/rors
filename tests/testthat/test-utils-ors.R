test_that("ors_ready() works", {
  server <- "pub"
  if (is_mock_test()) {
    app <- webfakes::new_app()
    app$get("/ors/v2/health", function(req, res) {
      counter <- res$app$locals$counter
      if (is.null(counter)) counter <- 0
      ready <- ifelse(counter == 0, "ready", "not ready")
      res$app$locals$counter <- counter + 1
      res$send_json(
        list(status = ready),
        auto_unbox = TRUE
      )
    })
    web <- webfakes::local_app_process(app, start = TRUE)
    server <- web$url()

    ors <- ors_instance(server = server)

    expect_true(ors_ready())
    expect_true(ors_ready(force = FALSE))
    expect_error(ors_ready(error = TRUE))
    expect_false(ors_ready())
  }
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
