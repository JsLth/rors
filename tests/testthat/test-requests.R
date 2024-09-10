# cran does not have java nor docker
skip_on_cran()

rors_cleanup()

# if on a tester machine, try to setup an ORS instance
# a real tester is a machine with env variable REAL_REQUESTS="true"
# otherwise, try to use mock tests
if (!is_mock_test()) {
  # if java is available, use it
  # otherwise, fall back to docker
  # if all fails, skip
  if (docker_running()) {
    ors <- local_ors_instance(
      dry = FALSE,
      verbose = FALSE,
      type = "docker"
    )
  } else if (has_valid_java()) {
    ors <- local_ors_instance(
      dry = FALSE,
      verbose = FALSE,
      type = "jar"
    )
  }

  ors$up()
} else {
  # set up a mock instance
  # no requests are actually sent
  # this just serves to let the functions think there is a server
  ors_instance(server = "http://localhost:8002/")
}

src <- test_coords(1:3)
dst <- test_coords(4:6)

with_mock_dir("directions", {
  describe("ors_pairwise()", {
    it("can compute distances between two datasets", {
      res <- ors_pairwise(src, dst)
      expect_s3_class(res, "data.frame")
      expect_type(res$distance, "double")
      expect_true(all(res$distance > 0))
    })
    it("can compute linestrings and format them as sf", {
      res <- ors_pairwise(src, dst, geometry = TRUE)
      expect_s3_class(res, "sf")
      expect_true(is_geometry_type(res, "LINESTRING"))
    })
    it("stores errors externally", {
      expect_warning(
        res <- ors_pairwise(src, dst, radiuses = 0),
        class = "ors_pairwise_fail_warning"
      )
      expect_warning(
        ors_pairwise(src, dst, radiuses = 20),
        class = "ors_pairwise_skip_warning"
      )
      expect_true(all(is.na(res$distance)))
      expect_length(last_ors_conditions(2), 2)
      err <- last_ors_conditions(2)
      expect_no_error(print(err))
      expect_equal(unique(err[[1]]$code), 2010)
      expect_match(err[[1]]$msg, " 20.0 meters")
      expect_match(err[[2]]$msg, " 0.0 meters")
    })
  })

  describe("ors_shortest_distances()", {
    it("routes from each point in src to each point in dst", {
      res <- ors_shortest_distances(src, dst, geometry = TRUE)
      expect_equal(nrow(res), 3)
      expect_equal(res$dest, c(2, 1, 2))
    })

    it("can work with grouped dataframes", {
      grp <- group_by_proximity(src, dst, n = 2)
      res <- ors_shortest_distances(src, grp, group = ".group")
      expect_equal(nrow(res), 3)
      expect_equal(res$dest, c(1, 2, 2))
    })
  })

  describe("ors_inspect()", {
  })

  describe("ors_accessibility()", {
    it("can compute isochrones in the right order", {
      res <- ors_accessibility(src)
      expect_s3_class(res, "sf")
      expect_equal(unique(table(res$group_index)), 2)
      expect_equal(unique(res$value), c(300, 200))
    })

    it("can compute and format intersections", {
      res <- ors_accessibility(src, intersections = TRUE)
      expect_s3_class(res, "sf")
      expect_named(res, c("a_index", "b_index", "a_range", "b_range", "geometry"))
    })

    it("can rasterize isochrones", {
      skip_if_not_installed("terra")
      res <- ors_accessibility(src, rasterize = TRUE, raster_resolution = c(10, 10))
      expect_s4_class(res, "SpatRaster")
      expect_equal(terra::ncol(res), 10)
      expect_equal(terra::nrow(res), 10)
      with_mocked_bindings(
        loadable = function(...) FALSE,
        expect_no_request(expect_error(
          ors_accessibility(src, rasterize = TRUE),
          class = "ors_loadable_error"
        ))
      )


    })
  })
})


with_mock_dir("matrix", {
  describe("ors_matrix()", {
    it("can take only one matrix", {
      res <- ors_matrix(src)
      expect_equal(dim(res), c(3, 3))
      expect_equal(diag(res), c(0, 0, 0))
    })

    it("can also take two matrices", {
      res <- ors_matrix(src, dst)
      expect_equal(dim(res), c(3, 3))
      expect_failure(expect_equal(diag(res), c(0, 0, 0)))
    })
  })
})


with_mock_dir("isochrones", {
  describe("ors_accessibility()", {

  })
})


rors_cleanup()
