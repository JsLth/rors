message("remote")

test_that("ors_token() works", {
  withr::with_envvar(
    c(ORS_TOKEN = ""),
    code = {
      expect_output(print(ors_token()), "requires no token", fixed = TRUE)
      expect_output(print(ors_token(TRUE)), "requires a token", fixed = TRUE)
      expect_output(print(ors_token(TRUE)), "No token found", fixed = TRUE)

      expect_length(capture.output(print(ors_token())), 2)
      expect_length(capture.output(print(ors_token(TRUE))), 3)
    }
  )

  withr::with_envvar(
    c(ORS_TOKEN = "test"),
    code = {
      expect_output(print(ors_token(TRUE)), "A token is stored", fixed = TRUE)
    }
  )
})


test_that("public api works", {
  withr::with_envvar(
    c(ORS_TOKEN = ""),
    code = {
      expect_error(
        ors_instance(server = "pub"),
        class = "ors_token_missing_error"
      )

      expect_error(
        ors_instance(server = "test.io", token = TRUE),
        class = "ors_token_missing_error"
      )
    }
  )

  expect_error(ors_instance(server = "test"), class = "ors_invalid_server_error")
})


test_that("token is properly used", {
  skip_if_offline("openrouteservice.org")
  skip_on_cran()
  withr::local_envvar(ORS_TOKEN = "notactuallyatoken")
  ors <- ors_instance(server = "pub")

  expect_true(is_ors_api(ors$url))
  expect_type(ors$get_status(), "list")
  expect_true(ors$is_ready())
  expect_true(ors$token)
  expect_true(attr(ors$token, "active"))

  sample <- sf::st_sfc(
    sf::st_point(c(1, 2)),
    sf::st_point(c(2, 3))
  )

  expect_warning(ors_pairwise(sample[1], sample[2]))
  cond <- last_ors_conditions()
  expect_match(cond[[1]]$msg, "Access to this API has been disallowed")
})


test_that("$.mount() always assigns to the correct environment", {
  ors1 <- ors_instance(server = "test1.org")
  ors2 <- ors_instance(server = "test2.org")
  expect_identical(ors2, get_instance())
  ors1$update()
  expect_identical(ors1, get_instance())
})


test_that("export fails early", {
  ors <- ors_instance(server = "pub")
  expect_error(sf::st_bbox(test_coords()), class = "ors_public_export_error")
})


rors_cleanup()
