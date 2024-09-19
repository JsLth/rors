print(basename(scriptName::current_source_filename()$value))

test_that("types are evaluated in error messages", {
  expect_error(assert_that(is_integerish(NULL)), regexp = "of type NULL")
  expect_error(assert_that(is_true_or_false("test")), regexp = "of type character")
})
