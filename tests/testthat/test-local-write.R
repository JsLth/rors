skip_if_offline("github.com")

ors <- local_ors_instance(
  verbose = TRUE,
  dry = TRUE,
  complete = TRUE,
  version = "7c77ae5"
)

test_that("$set_ram() works", {
  get_memory <- function(ors) {
    unlist(ors$compose$memory[c("init", "max")])
  }

  old <- get_memory(ors)

  expect_message(ors$set_ram())
  expect_type(get_memory(ors), "double")
  expect_failure(expect_identical(old, get_memory(ors)))

  expect_message(ors$set_ram(init = 0.5))
  expect_no_message(ors$set_ram(init = 0.5))
  expect_equal(get_memory(ors), c(init = 0.5, max = 0.5))

  expect_message(ors$set_ram(max = 1))
  expect_equal(get_memory(ors), c(init = 0.5, max = 1))

  expect_match(
    ors$compose$parsed$services$`ors-app`$environment[2],
    "-Xms500m",
    fixed = TRUE
  )

  expect_warning(ors$set_ram(1000))
})

test_that("$set_port() works", {
  get_ports <- function(ors) {
    ors$compose$ports$host
  }

  old <- get_ports(ors)

  expect_message(ors$set_port(8081), regexp = "8081")
  expect_no_message(ors$set_port(8081))
  expect_type(get_ports(ors), "character")
  expect_equal(get_ports(ors)[1],  "8081")
  expect_equal(get_ports(ors)[2], old[2])

  expect_message(ors$set_port(old))
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
  expect_message(ors$set_graphbuilding(TRUE))
  expect_message(ors$set_graphbuilding(FALSE))
})

test_that("$set_extract() works", {
  expect_message(ors$set_extract(file = system.file(
    "setup/monaco.pbf",
    package = "ORSRouting"
  )))

  expect_match(
    ors$compose$parsed$services$`ors-app`$volumes,
    regexp = "monaco.pbf",
    fixed = TRUE,
    all = FALSE
  )

  skip_if_offline("geofabrik.de")

  ors$set_extract("Rutland", provider = "geofabrik")
  expect_message(
    ors$set_extract("Rutland", provider = "geofabrik"),
    "already exists",
    fixed = TRUE
  )

  expect_match(
    ors$compose$parsed$services$`ors-app`$volumes,
    regexp = "rutland-latest",
    fixed = TRUE,
    all = FALSE
  )

  expect_error(
    ors$set_extract(file = "test.pbf"),
    "file does not exist",
    fixed = TRUE
  )

  unlink(ors$paths$extract)
  expect_warning(ors$update("self"), "could not be found", fixed = TRUE)
})

test_that("profiles work", {
  expect_in(
    c("hiking", "walking"),
    names(ors$config$parsed$ors$engine$profiles)
  )

  expect_message(
    ors$rm_profiles("hiking", "walking"),
    "hiking and walking",
    fixed = TRUE
  )

  expect_failure(expect_in(
    c("hiking", "walking"),
    names(ors$config$parsed$ors$engine$profiles)
  ))

  expect_message(
    ors$add_profiles(ors_profile("hiking"), "walking"),
    "foot-hiking and foot-walking",
    fixed = TRUE
  )

  expect_in(
    c("hiking", "walking"),
    names(ors$config$parsed$ors$engine$profiles)
  )
})

test_that("ors_profile() works", {
  expect_named(ors_profile(), "profile_default")
  expect_identical(ors_profile("walking"), ors_profile("foot-walking"))

  expect_error(ors_profile("test"))
  expect_named(ors_profile("test", template = FALSE)$test, c("profile", "enabled"))
  cstm <- ors_profile(c("name", "title"), template = FALSE)
  expect_named(cstm, "title")
  expect_identical(cstm$title$profile, "name")
})
