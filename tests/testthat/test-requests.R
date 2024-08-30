# if on a tester machine, try to setup an ORS instance
# otherwise, try to use mock tests
if (!on_real_tester()) {
  # if java is available, use it
  # otherwise, fall back to docker
  # if all fails, skip
  if (has_valid_java()) {
    ors <- local_ors_instance(dry = FALSE, verbose = FALSE, type = "jar")
  } else if (docker_running()) {
    ors <- local_ors_instance(dry = FALSE, verbose = FALSE)
  } else {
    skip("neither docker or jdk available -- cannot start ors instance")
  }

  ors$set_extract(file = test_pbf())
  ors$set_port()
}

src <- test_coords(1:3)
dst <- test_coords(4:6)

with_mock_api("directions", {
  describe("ors_pairwise()", {
    it("can compute distances between two datasets", {
      res <- ors_pairwise(src, dst)
      expect_s3_class(res, "data.frame")
      expect_type(res$distances, "numeric")
      expect_gt(res$distances, 0)
    })
    it("can compute linestrings and format them as sf", {
      res <- ors_pairwise(src, dst, geometry = TRUE)
      expect_s3_class(res, "sf")
      expect_true(is_geometry_type(res, "LINESTRING"))
    })
    it("stores errors externally", {
      res <- ors_pairwise(src, dst, radiuses = 0)
      expect_true(all(is.na(res$distance)))
      err <- last_ors_conditions()
      expect_no_error(print(err))
      expect_equal(unique(err[[1]]$code), 2010)
      expect_match(t[[1]]$msg, "0.0 meters")
    })
  })
})
