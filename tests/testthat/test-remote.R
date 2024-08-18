skip_if_offline("openrouteservice.org")
skip_if(!loadable("withr"))

test_that("public api works", {
  withr::with_envvar(
    c(ORS_TOKEN = ""),
    code = {
      expect_error(
        ors_instance(server = "pub"),
        "requires an API token",
        fixed = TRUE
      )

      expect_error(
        ors_instance(server = "test.io", token = TRUE),
        "requires an API token",
        fixed = TRUE
      )
    }
  )

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

  expect_error(ors_instance(server = "test"), "not a valid URL", fixed = TRUE)

  withr::local_envvar(ORS_TOKEN = "notactuallyatoken")
  ors <- ors_instance(server = "pub")

  expect_true(is_ors_api(ors$url))
  expect_type(ors$get_status(), "character")
  expect_true(ors$is_ready())
  expect_true(ors$token)
  expect_true(attr(ors$token, "active"))

  sample <- sf::st_sfc(
    sf::st_point(c(1, 2)),
    sf::st_point(c(2, 3))
  )

  expect_warning(ors_pairwise(sample[1], sample[2]))
  cond <- last_ors_conditions()
  expect_match(cond[[1]]$conditions, "Access to this API has been disallowed")
})
