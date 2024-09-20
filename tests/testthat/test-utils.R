message("utils")

test_that("%||% works", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
})

test_that("%NA% works", {
  expect_equal(1 %NA% 2, 1)
  expect_equal(NA %NA% 2, 2)
  expect_equal(NULL %NA% 2, 2)
  expect_equal(c(1, NA) %NA% 2, c(1, NA))
  expect_equal(c(NA, NA) %NA% 2, 2)
})

test_that("%empty%", {
  expect_equal(1 %empty% 2, 1)
  expect_equal(list() %empty% 2, 2)
  expect_equal(NULL %empty% 2, 2)
  expect_equal(NA %empty% 2, NA)
})

test_that("file_path_up works", {
  home <- normalizePath("~", "/")
  path1 <- file.path(home, "first")
  path2 <- file.path(path1, "second")
  root <- ifelse(is_windows(), "C:/", "/")

  expect_equal(file_path_up(path1), home)
  expect_equal(file_path_up(path2, 2), home)
  expect_equal(file_path_up(path1, 0), path1)
  expect_equal(file_path_up(path1, 10), root)
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

test_that("version detection works", {
  ver1 <- "7.2.0"
  ver2 <- "7c77ae5"
  ver3 <- "master"

  expect_true(is_numver(ver1))
  expect_false(is_numver(ver2))
  expect_false(is_numver(ver3))

  expect_true(is_version_desc("master"))
  expect_false(is_version_desc("master", "dh"))
  expect_true(is_version_desc("latest", "dh"))
  expect_false(is_version_desc("nightly", "gh"))
  expect_true(is_version_desc("master", "gh"))
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
  df3 <- data.frame()
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

  expect_identical(rbind_list(list(df2, df3)), df2)
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

  lst4 <- lst1
  lst4$b$x <- c("a", "b", "c")

  lst5 <- lst4
  names(lst5) <- c("", "b")

  lst6 <- list("a", "b")
  lst7 <- list(a = data.frame(a = 1))

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

  flst4 <- flatten_list(lst4)
  expect_named(flst4, c("a", "b.x", "b.y"))
  expect_length(flst4$b.x, 3)
  expect_type(flst4$b.x, "character")

  expect_named(flatten_list(lst5), c("", "b.x", "b.y"))
  expect_identical(flatten_list(lst6), lst6)
  expect_identical(flatten_list(lst7)$a, data.frame(a = 1))
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
  expect_type(mem$total, "double")
  expect_type(mem$free, "double")

  if (is_linux()) {
    expect_type(is_root(), "logical")
  }
})

test_that("is_true_or_false works", {
  expect_true(is_true_or_false(TRUE))
  expect_true(is_true_or_false(FALSE))
  expect_false(is_true_or_false(NA))
  expect_false(is_true_or_false(logical()))
})

test_that("is_sf works", {
  withr::local_package("sf")

  sf1 <- st_sfc()
  sf2 <- st_sf(sf1)

  expect_true(is_sf(sf1))
  expect_true(is_sf(sf2))
  expect_false(is_sf(sf1, sfc = FALSE))
})

test_that("is_geometry_type works", {
  withr::local_package("sf")

  sf1 <- st_sfc(st_point())
  sf2 <- st_sfc(st_point(), st_polygon())
  sf3 <- st_sfc(st_point(), st_polygon(), st_linestring())

  expect_false(is_geometry_type(sf1, c("POINT", "POLYGON")))
  expect_true(is_geometry_type(sf1, c("POINT", "POLYGON"), exclusive = FALSE))
  expect_true(is_geometry_type(sf2, c("POINT", "POLYGON"), exclusive = FALSE))
  expect_false(is_geometry_type(sf2, "POINT"))
  expect_true(is_geometry_type(sf2, "POINT", strict = FALSE))
})
