test_that("allocation_tier constructor validates arguments correctly", {
  expect_error(allocation_tier(), "allocation tier must have a non-empty 'name'")
  expect_error(allocation_tier(""), "allocation tier must have a non-empty 'name'")

  tr <- allocation_tier(
    "pub",
    source_filter = list(type = "public"),
    method = "gravity"
  )
  expect_s3_class(tr, "imumap_allocation_tier")
  expect_equal(tr$name, "pub")
  expect_equal(tr$method, "gravity")

  expect_error(
    allocation_tier("tr", method = "invalid_method"),
    "'arg' should be one of"
  )
})

test_that("allocation_tier print method works as expected", {
  tr <- allocation_tier("public", source_filter = list(type = "public"))
  out <- capture.output(print(tr))
  expect_true(any(grepl("<imumap_allocation_tier: public>", out)))
  expect_true(any(grepl("Method:.*gravity", out)))
})

test_that("validate_tiers enforces disjoint partitions and full coverage", {
  src <- data.frame(id = c("s1", "s2", "s3"), type = c("pub", "pub", "priv"))

  t_pub <- allocation_tier("public", source_filter = list(type = "pub"))
  t_priv <- allocation_tier("private", source_filter = list(type = "priv"))

  # Success: full disjoint partition
  idx_list <- imuMAP:::validate_tiers(list(t_pub, t_priv), src)
  expect_equal(idx_list$public, c(1, 2))
  expect_equal(idx_list$private, 3)

  # Error: duplicate names
  t_dup <- allocation_tier("public", source_filter = list(type = "priv"))
  expect_error(
    imuMAP:::validate_tiers(list(t_pub, t_dup), src),
    "duplicate tier names found: public"
  )

  # Error: unassigned sources
  expect_error(
    imuMAP:::validate_tiers(list(t_pub), src),
    "1 source locations were not assigned to any tier"
  )

  # Error: overlapping sources
  t_all <- allocation_tier("all", source_filter = NULL)
  expect_error(
    imuMAP:::validate_tiers(list(t_pub, t_all), src),
    "source 's1' was assigned to multiple tiers"
  )
})
