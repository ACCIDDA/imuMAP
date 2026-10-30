test_that("calc_iterative_weights decrements residual capacity across tiers", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 0)),
    crs = 3857
  )
  src <- sf::st_sf(
    id = c("pub_1", "priv_1"),
    school_type = c("Public", "Private"),
    district = c("d1", "none"),
    geometry = pts_src
  )

  pts_tgt <- sf::st_sfc(
    sf::st_point(c(0, 1)),
    sf::st_point(c(0, 1)),
    crs = 3857
  )
  tgt <- sf::st_sf(
    id = c("c1", "c2"),
    district = c("d1", "d2"),
    geometry = pts_tgt
  )

  # Distance matrix: equidistant to both targets
  dmat <- calc_distance_matrix(src, tgt)

  src_pop <- data.frame(id = c("pub_1", "priv_1"), population = c(80, 60))
  tgt_pop <- data.frame(id = c("c1", "c2"), population = c(100, 100))

  # Tier 1: Public school restricted to district d1 (only c1)
  r_pub <- eligibility_rule(source_filter = list(school_type = "Public"), match_cols = "district")
  dmat_elig <- calc_distance_matrix(src, tgt, eligibility = r_pub)

  t1 <- allocation_tier("public", source_filter = list(school_type = "Public"), kernel_options = kernel_options("uniform"))
  t2 <- allocation_tier("private", source_filter = list(school_type = "Private"), kernel_options = kernel_options("uniform"))

  res <- calc_iterative_weights(
    dist_matrix = dmat_elig,
    source_pop = src_pop,
    target_pop = tgt_pop,
    tiers = list(t1, t2),
    source_data = src
  )

  expect_s3_class(res, "imumap_iterative_weights")
  expect_s3_class(res, "imumap_weights")

  # Conservation of weights: every source row sums to 1
  expect_equal(as.numeric(Matrix::rowSums(res$weights)), c(1, 1))

  # Public school allocated exclusively to c1 (80 students)
  expect_equal(as.numeric(res$weights["pub_1", "c1"]), 1)
  expect_equal(as.numeric(res$weights["pub_1", "c2"]), 0)

  # Target 1 residual capacity after Tier 1: 100 - 80 = 20.
  # Target 2 residual capacity after Tier 1: 100 - 0 = 100.
  # In Tier 2, private school (uniform distance) allocates proportionally to (20, 100):
  # weight c1 = 20 / 120 = 1/6, weight c2 = 100 / 120 = 5/6.
  expect_equal(as.numeric(res$weights["priv_1", "c1"]), 20 / 120)
  expect_equal(as.numeric(res$weights["priv_1", "c2"]), 100 / 120)

  # Final residual capacity:
  # c1: 20 - (60 * 1/6) = 10
  # c2: 100 - (60 * 5/6) = 50
  expect_equal(unname(res$residual_capacity["c1"]), 10)
  expect_equal(unname(res$residual_capacity["c2"]), 50)
})

test_that("calc_iterative_weights supports optimal transport solver", {
  skip_if_not_installed("transport")

  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(10, 0)),
    crs = 3857
  )
  src <- sf::st_sf(
    id = c("s1", "s2"),
    type = c("pub", "priv"),
    geometry = pts_src
  )

  pts_tgt <- sf::st_sfc(
    sf::st_point(c(0, 1)),
    sf::st_point(c(10, 1)),
    crs = 3857
  )
  tgt <- sf::st_sf(
    id = c("c1", "c2"),
    geometry = pts_tgt
  )

  dmat <- calc_distance_matrix(src, tgt)
  src_pop <- data.frame(id = c("s1", "s2"), population = c(50, 40))
  tgt_pop <- data.frame(id = c("c1", "c2"), population = c(100, 100))

  t1 <- allocation_tier("pub", source_filter = list(type = "pub"), method = "transport")
  t2 <- allocation_tier("priv", source_filter = list(type = "priv"), method = "transport")

  res <- calc_iterative_weights(
    dist_matrix = dmat,
    source_pop = src_pop,
    target_pop = tgt_pop,
    tiers = list(t1, t2),
    source_data = src
  )

  expect_s3_class(res, "imumap_iterative_weights")
  expect_equal(as.numeric(Matrix::rowSums(res$weights)), c(1, 1))

  # Transport should map s1 to nearest c1, and s2 to nearest c2
  expect_equal(as.numeric(res$weights["s1", "c1"]), 1)
  expect_equal(as.numeric(res$weights["s2", "c2"]), 1)
})

test_that("calc_iterative_weights works with downstream allocate()", {
  pts_src <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(0, 0)), crs = 3857)
  src <- sf::st_sf(id = c("s1", "s2"), type = c("a", "b"), geometry = pts_src)
  pts_tgt <- sf::st_sfc(sf::st_point(c(0, 1)), sf::st_point(c(0, 2)), crs = 3857)
  tgt <- sf::st_sf(id = c("c1", "c2"), geometry = pts_tgt)

  dmat <- calc_distance_matrix(src, tgt)
  src_pop <- data.frame(id = c("s1", "s2"), population = c(100, 100))
  tgt_pop <- data.frame(id = c("c1", "c2"), population = c(200, 200))

  t1 <- allocation_tier("t1", source_filter = list(type = "a"))
  t2 <- allocation_tier("t2", source_filter = list(type = "b"))

  w <- calc_iterative_weights(dmat, src_pop, tgt_pop, list(t1, t2), source_data = src)

  cov_data <- data.frame(
    id = c("s1", "s2"),
    vaccinated = c(80, 90),
    total = c(100, 100)
  )

  res_alloc <- allocate(cov_data, w)
  expect_s3_class(res_alloc, "data.table")
  expect_equal(res_alloc$id, c("c1", "c2"))
  expect_equal(sum(res_alloc$vaccinated), 170)
  expect_equal(sum(res_alloc$total), 200)
})

test_that("print and summary methods for iterative weights produce formatted output", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  src <- sf::st_sf(id = "s1", type = "a", geometry = pts)
  tgt <- sf::st_sf(id = "c1", geometry = pts)
  dmat <- calc_distance_matrix(src, tgt)
  src_pop <- data.frame(id = "s1", population = 50)
  tgt_pop <- data.frame(id = "c1", population = 100)

  t1 <- allocation_tier("tier_one", source_filter = list(type = "a"))
  w <- calc_iterative_weights(dmat, src_pop, tgt_pop, list(t1), source_data = src)

  out_print <- capture.output(print(w))
  expect_true(any(grepl("<imumap_iterative_weights>", out_print)))
  expect_true(any(grepl("Tiers:.*tier_one", out_print)))

  sum_obj <- summary(w)
  expect_s3_class(sum_obj, "summary_imumap_iterative_weights")
  out_sum <- capture.output(print(sum_obj))
  expect_true(any(grepl("imuMAP Multi-Tier Allocation Summary", out_sum)))
  expect_true(any(grepl("tier_one", out_sum)))
})
