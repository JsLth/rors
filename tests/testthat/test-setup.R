skip_if_offline("github.com")

tdir <- file.path(normalizePath(tempdir, "/"), "ors-test")
dir.create(tdir, recursive = TRUE)
on.exit(unlink(tdir, recursive = TRUE))
withr::local_options(ors_docker_dry = TRUE)

expect_silent(ors <- ors_instance(tdir, verbose = FALSE))

test_that("setup is created properly", {
  expect_true(file.exists(file.path(tdir, "docker-compose.yml")))
  expect_true(file.exists(file.path(tdir, "docker/data/monaco.pbf")))
})

test_that("instance is mounted", {
  expect_true(any_mounted())
  expect_no_error(check_instance())
  expect_equal(get_id(), "ors-app")
})

test_that("compose is parsed correctly", {
  expect_no_error(capture_output(print(ors$compose)))
  expect_true(all(vapply(ors$compose$ports, is.character, logical(1))))
  expect_type(unlist(ors$compose$memory), "double")
})

test_that("paths are parsed correctly", {
  expect_true(file.exists(ors$paths$compose))
  expect_true(dir.exists(ors$paths$top))
  expect_null(ors$paths$config)
  expect_null(ors$paths$extract)
  expect_no_error(capture_output(print(ors$paths)))
})

test_that("writing works", {
  og <- readLines(file.path(tdir, "docker-compose.yml"), warn = FALSE)
  og <- gsub("\\s*#.+$", "", og) # remove comments
  og <- og[nchar(og) > 0]
  ors$update()
  new <- readLines(file.path(tdir, "docker-compose.yml"), warn = FALSE)
  expect_length(waldo::compare(og, new), 0)
})

expect_warning(
  ors$set_extract(file = system.file("setup/monaco.pbf", package = "ORSRouting"))
)
dir.create(file.path(tdir, "docker", "data"))
ors$set_extract(file = system.file("setup/monaco.pbf", package = "ORSRouting"))

test_that("set_ram() works", {
  ors$set_ram()
})
