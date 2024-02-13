test_that("ors_ready() works", {
  server <- "pub"
  if (is_mock_test) {
    app <- webfakes::new_app()
    app$get("/ors/v2/health", function(req, res) {
      res$send_json(
        list(status = "ready"),
        auto_unbox = TRUE
      )
    })
    web <- webfakes::local_app_process(app, start = TRUE)
    server <- web$url()
  }

  ors <- ors_instance(server = server)

})
