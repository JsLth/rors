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
  }

  # httr2::request(server) %>% httr2::req_method("GET") %>%
  # httr2::req_url_path("ors/v2/health") %>% httr2::req_perform() %>%
  # httr2::resp_body_json()

  ors <- ors_instance(server = server)

  expect_true(ors_ready())
  expect_true(ors_ready(force = FALSE))
  expect_error(ors_ready(error = TRUE))
  expect_false(ors_ready())
})
