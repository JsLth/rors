message("local-parse")

skip_if_offline("github.com")

ors <- local_ors_instance(verbose = FALSE, dry = TRUE)
ors$set_extract(file = test_pbf())

test_that("printing produces non-standard output", {
  # if a print method is found, use it
  # otherwise, omit attributes and print.default
  print_no_attributes <- function(x, ...) {
    cls <- class(x)[[1]]
    method <- get0(
      sprintf("print.%s", cls),
      envir = asNamespace("rors"),
      ifnotfound = print.default
    )
    attributes(x) <- NULL
    method(x)
    invisible(x)
  }

  expect_failure(expect_equal(
    capture.output(print(ors$extract)),
    capture.output(print_no_attributes(unclass(ors$extract)))
  ))
  expect_failure(expect_equal(
    capture.output(print(ors$compose)),
    capture.output(print_no_attributes(unclass(ors$compose)))
  ))
  expect_failure(expect_equal(
    capture.output(print(ors$config)),
    capture.output(print_no_attributes(unclass(ors$config)))
  ))
  expect_failure(expect_equal(
    capture.output(print(ors$compose$parsed)),
    capture.output(print_no_attributes(unclass(ors$compose$parsed)))
  ))
  expect_failure(expect_equal(
    capture.output(print(ors$config$parsed)),
    capture.output(print_no_attributes(unclass(ors$config$parsed)))
  ))
  expect_failure(expect_equal(
    capture.output(print(ors$report())),
    capture.output(print_no_attributes(unclass(ors$report())))
  ))
})

test_that("setup is created properly", {
  expect_true(file.exists(ors$paths$compose))
})

test_that("new instances of other types are blocked", {
  skip_if_not(has_valid_java(), "java unavailable")

  expect_error(
    without_internet(ors_instance(
      dir = dirname(ors$paths$top),
      version = "8.0.0",
      verbose = FALSE,
      type = "jar"
    )),
    class = "ors_wrong_ors_type_error"
  )
})

test_that("instance is mounted", {
  expect_true(any_mounted())
  expect_no_error(check_instance())
})

test_that("compose is parsed correctly", {
  expect_s3_class(ors$compose, "ors_settings")
  expect_s3_class(ors$compose$parsed, "ors_compose_parsed")
  expect_no_error(capture_output(print(ors$compose)))
  expect_s3_class(ors$compose$ports, "data.frame")
  expect_true(all(vapply(ors$compose$ports, is.character, logical(1))))
  expect_type(unlist(ors$compose$memory), "double")
  expect_type(ors$compose$name, "character")
  expect_type(ors$compose$image, "character")
  expect_true(is_true_or_false(ors$compose$rebuild_graphs))
})

test_that("paths are parsed correctly", {
  expect_true(file.exists(ors$paths$compose))
  expect_true(dir.exists(ors$paths$top))
  expect_true(file.exists(ors$paths$config))
  expect_true(file.exists(ors$paths$extract))
  expect_output(print(ors$paths), "<- compose", fixed = TRUE)
})

test_that("writing works", {
  og <- readLines(ors$paths$compose, warn = FALSE)
  og <- gsub("\\s*#.+$", "", og) # remove comments
  og <- og[nchar(og) > 0]
  ors$update()
  new <- readLines(ors$paths$compose, warn = FALSE)
  expect_true(identical(og, new))
})

test_that("extract is parsed correctly", {
  expect_identical(ors$extract$name, "monaco.pbf")
  expect_type(ors$extract$size, "double")
  expect_output(print(ors$paths), "<- extract", fixed = TRUE) # TODO
})

test_that("config is parsed correctly", {
  expect_s3_class(ors$config, "ors_config")
  expect_s3_class(ors$config$parsed, "ors_config_parsed")
  expect_named(ors$config$profiles)
  expect_output(print(ors$paths), "<- config", fixed = TRUE)
})

test_that("version errors are informative", {
  expect_error(
    with_ors_instance({}, version = "notaversion"),
    class = "ors_version_404_error"
  )
})

test_that("$.mount() always assigns to the correct environment", {
  dir.create(d1 <- file.path(tempdir(), "test1"))
  dir.create(d2 <- file.path(tempdir(), "test2"))
  ors1 <- local_ors_instance(dir = d1)
  ors2 <- local_ors_instance(dir = d2)
  ors1$set_name("test1")
  ors2$set_name("test2")
  expect_identical(ors2, get_instance())
  ors1$update()
  expect_identical(ors1, get_instance())
})


rors_cleanup()
