test_that("verifying works", {
  expect_message(
    param_verify(c(type = FALSE), "avoid_polygons"),
    regexp = "avoid_polygons skipped",
    fixed = TRUE
  )

  expect_message(
    param_verify(c(type = FALSE), "avoid_polygons"),
    regexp = "invalid type",
    fixed = TRUE
  )

  expect_message(
    param_verify(c(type = FALSE, match = FALSE), "avoid_polygons"),
    regexp = "value is not defined",
    fixed = TRUE
  )

  expect_true(
    param_verify(c(type = TRUE), "avoid_polygons")
  )
})

test_that("alternative_routes", {
  p1 <- list(alternative_routes = list(target_count = 2))
  p2 <- list(alternative_routes = list(test = 2))
  p3 <- list(alternative_routes = list(test = 2, share_factor = 2))

  # defined params should work
  expect_equal(prepare_ors_params(p1, "driving-car"), p1, ignore_attr = TRUE)

  # profile shouldnt matter
  expect_equal(prepare_ors_params(p1, "wheelchair"), p1, ignore_attr = TRUE)

  # undefined params shouldnt work
  expect_warning(
    prepare_ors_params(p2, "driving-car"),
    "formatted incorrectly",
    fixed = TRUE
  )

  # undefined params combined with params should throw a different warning
  expect_warning(
    prepare_ors_params(p3, "driving-car"),
    "are not known",
    fixed = TRUE
  )
})

test_that("vector params work", {
  v1 <- list(geometry_simplify = TRUE)
  v2 <- list(geometry_simplify = c(TRUE, TRUE))
  v3 <- list(geometry_simplify = "true")
  v4 <- list(continue_straight = TRUE)
  v5 <- list(avoid_countries = 34)

  expect_equal(prepare_ors_params(v1, profile = "driving-car"), v1, ignore_attr = TRUE)
  expect_equal(prepare_ors_params(v4, profile = "driving-car"), v4, ignore_attr = TRUE)
  expect_equal(prepare_ors_params(v5, profile = "driving-car"), v5, ignore_attr = TRUE)
  expect_warning(prepare_ors_params(v2, profile = "driving-car"))
  expect_warning(prepare_ors_params(v3, profile = "driving-car"))
})
