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
  print(identify_extract(get_instance()$paths$top))
  get_extract_boundaries()

  ors <- ors_instance(server = "public")
  print(ors_is_local(ors))
  get_extract_boundaries()
})


rors_cleanup()
