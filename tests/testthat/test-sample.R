message("sample")

skip_if_not(sf::sf_extSoftVersion()["GDAL"] > "2.1.0")
skip_on_cran()

ors <- local_ors_instance(verbose = FALSE)
ors$set_extract(file = test_pbf())

test_that("extract is read and processed properly", {
  with_mocked_bindings(identify_extract = function(...) test_pbf(), {
    bounds <- get_extract_boundaries()
    expect_true(sf::st_is(bounds, "MULTIPOLYGON"))
    expect_length(bounds, 1)

    exists("extract_boundaries", envir = getFromNamespace("rors_cache", ns = "rors"))

    expect_no_error(get_extract_boundaries())
    expect_s3_class(ors_sample(2), "sfc")
  })
})


test_that("fails properly", {
  ors$rm_extract()
  expect_error(get_extract_boundaries(force = TRUE), class = "ors_extract_not_found_error")

  ors <- ors_instance(server = "public")
  expect_error(get_extract_boundaries(force = TRUE), class = "ors_remote_sample_error")
})


rors_cleanup()
