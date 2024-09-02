src <- test_coords(1:3)
dst <- test_coords(4:6)

describe("group_by_proximity()", {
  it("can group by n", {
    res <- group_by_proximity(src, dst, n = 2)
    expect_equal(unique(res$.group), c(1, 2, 3))
    expect_equal(as.vector(table(res$.group)), c(2, 2, 2))
  })

  it("can group by distance", {
    res <- group_by_proximity(src, dst, radius = 1000)
    expect_equal(unique(res$.group), c(1, 2, 3))
    expect_equal(as.vector(table(res$.group)), c(3, 3, 2))
  })

  it("overwrites the .group column", {
     res <- group_by_proximity(src, dst, n = 2)
     expect_named(group_by_proximity(res, dst, n = 2), c(".group", "geometry"))
     expect_named(group_by_proximity(res, dst, radius = 1000), c(".group", "geometry"))
  })

  it("must have n < length(dst)", {
    expect_warning(group_by_proximity(src, dst, n = 4), class = "ors_group_n_warn")
  })

  it("must have a grouping method specified", {
    expect_error(group_by_proximity(src, dst), class = "ors_group_method_error")
  })

  it("must have a valid radius", {
    expect_error(group_by_proximity(src, dst, n = 2, radius = "test"), class = "ors_group_invalid_radius_error")
  })
})
