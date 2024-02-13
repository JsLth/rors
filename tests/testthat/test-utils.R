test_that("%||% works", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
})

test_that("file_path_up works", {
  home <- normalizePath("~", "/")
  path1 <- file.path(home, "first")
  path2 <- file.path(path1, "second")

  expect_equal(file_path_up(path1), home)
  expect_equal(file_path_up(path2, 2), home)
  expect_equal(file_path_up(path1, 0), character())
  expect_equal(file_path_up(path1, 10), character())
})

test_that("url tools work", {
  url1 <- "localhost:8080"
  url2 <- "127.0.0.1:8080"
  url3 <- "https://localhost:8080/path/to/sth"
  url4 <- "google.com"
  url5 <- "localhost"
  url6 <- "https://api.openrouteservice.org/"

  expect_true(is_local(url1))
  expect_true(is_local(url2))
  expect_true(is_local(url3))
  expect_false(is_local(url4))
  expect_false(is_local(url5))
  expect_false(is_local(url6))

  expect_true(is_url(url1))
  expect_true(is_url(url2))
  expect_true(is_url(url3))
  expect_true(is_url(url4))
  expect_false(is_url(url5))
  expect_true(is_url(url6))

  expect_false(is_ors_api(url1))
  expect_false(is_ors_api(url2))
  expect_false(is_ors_api(url3))
  expect_false(is_ors_api(url4))
  expect_false(is_ors_api(url5))
  expect_true(is_ors_api(url6))
})

test_that("is_numer works", {
  ver1 <- "7.2.0"
  ver2 <- "7c77ae5"
  ver3 <- "master"

  expect_true(is_numver(ver1))
  expect_false(is_numver(ver2))
  expect_false(is_numver(ver3))
})

test_that("relative_path works", {
  path1 <- normalizePath("~", "/")
  path2 <- file.path(path1, "test")

  expect_equal(relative_path(path2, path1), "test")
  expect_equal(relative_path(path1, path1), ".")
  expect_equal(relative_path(path1, path2), normalizePath("~", "/"))
})

test_that("character tools work", {
  expect_equal(capitalize_char("test"), "Test")
  expect_equal(regex_match(c("test1", "test2"), "[0-9]"), list("1", "2"))
})

test_that("rbind_list works", {
  withr::local_package("sf")
  df1 <- data.frame(a = 1, b = "a")
  df2 <- data.frame(a = 2, c = "b")
  sf1 <- st_sf(
    a = c(1, 2),
    geometry = st_sfc(st_point(c(1, 2)), st_point(c(5, 8)))
  )
  sf2 <- st_sf(
    b = c(3, 4),
    geometry = st_sfc(st_point(c(4, 5)), st_point(c(3, 5)))
  )

  no_sf <- rbind_list(list(df1, df2))
  no_crs <- rbind_list(list(sf1, sf2))

  expect_length(no_sf$a, 2)
  expect_equal(no_sf$b, c("a", NA))
  expect_s3_class(no_crs, "sf")
  expect_identical(st_crs(no_crs), NA_crs_)

  st_crs(sf1) <- 4326
  expect_error(rbind_list(list(sf1, sf2)))

  st_crs(sf2) <- 4326
  expect_no_error(rbind_list(list(sf1, sf2)))
})

test_that("list tools work", {
  lst1 <- list(
    a = 1,
    b = list(
      x = "a",
      y = 2
    )
  )

  lst2 <- list(
    a = 1,
    b = list(
      x = "b",
      y = 2,
      z = TRUE,
      v = NULL
    ),
    c = 10
  )

  lst3 <- lst1
  lst3["d"] <- list(NULL)

  mod <- modify_list(lst1, lst2)

  expect_equal(mod$c, 10)
  expect_equal(mod$b$x, "b")
  expect_equal(mod$b$y, 2)
  expect_equal(mod$b$z, TRUE)
  expect_false("v" %in% names(mod$b))

  expect_false(equivalent_list(lst1, lst2))
  expect_true(equivalent_list(lst1, lst1))
  expect_true(equivalent_list(lst1, lst3))

  expect_equal(box(1), list(1))
  expect_equal(box(list(1)), list(1))
})

test_that("decode_base2 works", {
  expect_error(suppressWarnings(decode_base2(-1)))
  expect_equal(decode_base2(3), c(2, 1))
  expect_equal(decode_base2(8), 8)
  expect_null(decode_base2(2.5))
})

test_that("dataframe nesting works", {
  df1 <- data.frame(x = c(1, 1), y = c(1, 2))
  df2 <- data.frame(x = c(5, 4), y = c(4, 4))

  nest <- df_nest(a = df1, b = df2)
  expect_equal(nest, nest[c("a", "b")])
})

test_that("sys tools work", {
  mem <- get_memory_info()
  expect_s3_class(mem$total, "double")
  expect_s3_class(mem$free, "double")
})
