test_that("verifying works", {
  expect_identical(param_verify(c(type = FALSE), "avoid_polygons"), "invalid type")
  expect_length(param_verify(c(type = FALSE, match = FALSE), "avoid_polygons"), 2)
  expect_null(param_verify(c(type = TRUE), "avoid_polygons"))
})

test_that("fails early", {
  p1 <- list(radiuses = 1, radiuses = 1)
  p2 <- list(test = 1)
  p3 <- list(alternative_routes = list(test = 2))

  # duplicated params shouldnt work
  expect_error(prepare_ors_params(p1, "driving-car"), class = "ors_param_duplicated_error")

  # undefined params shouldnt work
  expect_error(prepare_ors_params(p2, "driving-car"), class = "ors_param_unknown_error")

  # undefined nested params shouldnt work
  expect_error(prepare_ors_params(p3, "driving-car"), class =  "ors_param_invalid_child_error")
})

test_that("param checking works", {
  v1 <- list(geometry_simplify = TRUE)
  v2 <- list(geometry_simplify = c(TRUE, TRUE))
  v3 <- list(geometry_simplify = "true")
  v4 <- list(avoid_countries = 34)
  v5 <- list(preference = "test")
  v6 <- list(avoid_polygons = withr::with_package("sf", sf::st_sfc(sf::st_cast(
    sf::st_linestring(matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0), ncol = 2)),
    "POLYGON"
  ), crs = 4326)))
  #v7 <- list(extra_info = c("osmid", "roadaccessrestrictions"))
  v8 <- list(extra_info = "osmid")

  # check if param checking can run successfully
  expect_equal(prepare_ors_params(v1, profile = "driving-car"), v1, ignore_attr = TRUE)

  # check if length checks work
  expect_error(prepare_ors_params(v2, profile = "driving-car"), "invalid length")

  # check if type checks work
  expect_error(prepare_ors_params(v3, profile = "driving-car"), "invalid type")

  # check if profile checks work
  expect_error(prepare_ors_params(v4, profile = "foot-walking"), "different profile")

  # check if match checks work
  expect_error(prepare_ors_params(v5, profile = "driving-car"), "undefined values")

  # check if sf checks work
  expect_equal(prepare_ors_params(v6, profile = "driving-car")$avoid_polygons$type, "FeatureCollection")
  expect_error(prepare_ors_params(list(avoid_polygons = 1), profile = "driving-car"), "invalid sf object")

  # check if extra_info checks work
  #expect_error(prepare_ors_params(v7, profile = "wheelchair"), "possibly wrong profile")
  expect_equal(unclass(prepare_ors_params(v8, profile = "wheelchair")), v8)
})


test_that("nested params recurse properly", {
  p1 <- list(alternative_routes = list(target_count = 2))
  p2 <- list(weightings = list(green = 3, shadow = TRUE), radiuses = "test")

  # recursion should work
  expect_equal(prepare_ors_params(p1, "driving-car"), p1, ignore_attr = TRUE)

  # all nested params should throw an error
  expect_length(expect_error(prepare_ors_params(p2, "foot-walking"))$body, 3)
})


test_that("bearings are formatted correctly", {
  m1 <- matrix(c(360, NA, 90, 10, 10, 10), ncol = 2)
  m2 <- c(360, 180, 90)
  m3 <- matrix(rep(NA_real_, 6), ncol = 2)
  m4 <- matrix(letters[1:6], ncol = 2)
  m5 <- m1
  colnames(m5) <- c("a", "b")
  rownames(m5) <- 1:3

  # check if bearings can be of mixed length
  expect_length(r <- prepare_ors_params(list(bearings = m1), "cycling-regular", n = 3)$bearings, 3)

  # check if bearings can be a vector
  expect_length(prepare_ors_params(list(bearings = m2), "cycling-regular", n = 3)$bearings, 3)

  # check if bearings can be all NA
  expect_length(prepare_ors_params(list(bearings = m3), "cycling-regular", n = 3)$bearings[[1]], 0)

  # check bearing type check
  expect_error(prepare_ors_params(list(bearings = m4), "cycling-regular", n = 3), class = "ors_param_invalid_error")

  # check if bearings are name-resistant
  expect_identical(unclass(prepare_ors_params(list(bearings = m5), "cycling-regular", n = 3))$bearings, r)
})
