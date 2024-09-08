skip_if_offline("github.com")
skip_on_cran()

ors <- local_ors_instance(
  verbose = TRUE,
  dry = TRUE,
  version = "8.0.0"
)
ors$set_extract(file = test_pbf())

test_that("$set_ram() works", {
  get_memory1 <- function(ors) {
    unlist(ors$compose$memory[c("init", "max")], use.names = FALSE)
  }

  get_memory2 <- function(ors) {
    unlist(
      ors$compose$parsed$services$`ors-app`$environment[c("XMS", "XMX")],
      use.names = FALSE
    )
  }

  old <- get_memory1(ors)

  expect_message(ors$set_memory())
  expect_type(get_memory1(ors), "double")
  expect_failure(expect_identical(old, get_memory1(ors)))

  expect_message(ors$set_memory(init = 0.5))
  expect_no_message(ors$set_memory(init = 0.5))
  expect_equal(get_memory1(ors), c(0.5, 0.5))
  expect_equal(get_memory2(ors), c("500m", "500m"))

  expect_message(ors$set_memory(max = 1))
  expect_equal(get_memory1(ors), c(0.5, 1))
  expect_equal(get_memory2(ors), c("500m", "1000m"))

  expect_warning(ors$set_memory(1000))
})

test_that("$set_port() works", {
  get_ports <- function(ors) {
    ors$compose$ports$host
  }

  old <- get_ports(ors)

  expect_message(ors$set_port(8081), regexp = "8081")
  expect_no_message(ors$set_port(8081))
  expect_type(get_ports(ors), "character")
  expect_equal(get_ports(ors)[1], "8081")
  expect_equal(get_ports(ors)[2], old[2])

  expect_message(ors$set_port(as.numeric(old)))
  expect_type(get_ports(ors), "character")
  expect_equal(get_ports(ors), old)

  expect_message(ors$set_port())
  expect_type(get_ports(ors), "character")
  expect_failure(expect_equal(old, get_ports(ors)))
})

test_that("$set_name() works", {
  get_name <- function(ors) {
    ors$compose$parsed$services$`ors-app`$container_name
  }

  expect_message(ors$set_name("test"), regexp = "test")
  expect_no_message(ors$set_name("test"))
  expect_identical(get_name(ors), "test")

  expect_message(ors$set_name(), regexp = "ors-app-.+$")
  expect_match(get_name(ors), "^ors-app-.+$")
})

test_that("$set_graphbuilding() works", {
  get_gp1 <- function(ors) {
    ors$compose$rebuild_graphs
  }
  get_gp2 <- function(ors) {
    ors$compose$parsed$services$`ors-app`$environment$REBUILD_GRAPHS
  }
  expect_true(get_gp1(ors))
  expect_true(get_gp2(ors))
  expect_message(ors$set_graphbuilding(FALSE))
  expect_false(get_gp1(ors))
  expect_false(get_gp2(ors))
  expect_message(ors$set_graphbuilding(TRUE))
  expect_true(get_gp1(ors))
  expect_true(get_gp2(ors))
  expect_no_message(ors$set_graphbuilding(TRUE))
})

test_that("$set_extract() works", {
  expect_s3_class(ors$extract, "ors_extract")
  expect_message(ors$set_extract(file = system.file(
    "setup/monaco.pbf",
    package = "rors"
  )))
  expect_s3_class(ors$extract, "ors_extract")
  expect_identical(ors$config$parsed$ors$engine$source_file, "files/monaco.pbf")

  skip_if_offline("geofabrik.de")

  ors$set_extract("Rutland", provider = "geofabrik")
  expect_message(
    ors$set_extract("Rutland", provider = "geofabrik"),
    "already exists",
    fixed = TRUE
  )

  expect_identical(
    ors$config$parsed$ors$engine$source_file,
    "files/geofabrik_rutland-latest.osm.pbf"
  )

  expect_error(
    ors$set_extract(file = "test.pbf"),
    class = "ors_extract_relative_error"
  )
  expect_warning(expect_error(
    ors$set_extract(file = "test/test.pbf"),
    class = "ors_extract_not_found_error"
  ))

  expect_message(
    ors$rm_extract(dir(
      file.path(ors$paths$top, "files"),
      pattern = "\\.pbf$"
    )),
    regexp = "<- active extract",
    fixed = TRUE
  )

  expect_null(ors$config$parsed$ors$engine$source_file)
  expect_null(ors$extract)

  # test if extract is actually unset or if it is still in compose
  expect_no_warning(ors$update("self"))
})

test_that("$add_profiles work", {
  get_profiles <- function(ors) {
    ors$config$parsed$ors$engine$profiles
  }

  expect_failure(expect_in(
    c("hiking", "walking"),
    names(get_profiles(ors))
  ))

  expect_message(
    ors$add_profiles(ors_profile("hiking"), "walking"),
    "hiking and walking",
    fixed = TRUE
  )

  expect_no_message(ors$add_profiles(ors_profile("hiking"), "walking"))

  expect_in(
    c("hiking", "walking"),
    names(get_profiles(ors))
  )

  expect_message(
    ors$rm_profiles("hiking", "walking"),
    "hiking and walking",
    fixed = TRUE
  )

  expect_no_message(ors$rm_profiles("hiking", "walking"))

  expect_failure(expect_in(
    c("hiking", "walking"),
    names(get_profiles(ors))
  ))
})

test_that("$set_endpoints works", {
  get_endpoints <- function(ors, which = NULL) {
    ep <- ors$config$parsed$ors$endpoints
    if (!is.null(which)) {
      ep[[which]]
    } else {
      ep
    }
  }

  matrix <- list(maximum_routes = 200, maximum_visited_nodes = 50000)
  isochr <- list(maximum_intervals = 1)

  expect_message(ors$set_endpoints(matrix = matrix, isochrone = isochr))
  expect_no_message(ors$set_endpoints(matrix = matrix, isochrone = isochr))

  expect_identical(get_endpoints(ors, "matrix"), matrix)
  expect_identical(get_endpoints(ors, "isochrone"), isochr)

  ors$set_endpoints(matrix = list(test = 5))
  expect_identical(get_endpoints(ors, "matrix"), c(matrix, test = 5))

  expect_warning(
    ors$set_endpoints(invalid = list(test = 5), matrix = list(test = 5)),
    class = "ors_excess_endpoint_warn"
  )

  expect_error(
    ors$set_endpoints(invalid = list(test = 5)),
    class = "ors_invalid_endpoint_error"
  )
})

test_that("$set_image works", {
  expect_identical(ors$compose$image, "v8.0.0")

  expect_message(ors$set_image("8.1.0"))
  expect_no_message(ors$set_image("8.1.0"))
  expect_message(ors$set_image("latest"))
  expect_no_message(ors$set_image("test"))
  expect_identical(ors$compose$image, "latest")
})

test_that("ors_profile() works", {
  expect_named(ors_profile(), "profile_default")
  expect_identical(ors_profile("walking"), ors_profile("foot-walking"))

  expect_error(ors_profile("test"), class = "ors_profile_template_error")
  expect_named(ors_profile("test", template = FALSE)$test, c("profile", "enabled"))
  cstm <- ors_profile(c("name", "title"), template = FALSE)
  expect_named(cstm, "title")
  expect_identical(cstm$title$profile, "name")
})
