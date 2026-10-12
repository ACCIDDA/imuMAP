test_that("calc_allocation_weights calculates normalized weights correctly", {
  pts_src <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  src <- sf::st_sf(id = "s1", geometry = pts_src)

  pts_snk <- sf::st_sfc(
    sf::st_point(c(0, 1)),
    sf::st_point(c(0, 2)),
    crs = 3857
  )
  snk <- sf::st_sf(id = c("c1", "c2"), geometry = pts_snk)

  dmat <- calc_distance_matrix(src, snk)
  w <- calc_allocation_weights(dmat, kernel_options = kernel_options("uniform"))

  expect_s3_class(w, "imumap_weights")
  expect_equal(as.numeric(Matrix::rowSums(w$weights)), 1)
  expect_equal(as.numeric(w$weights["s1", "c1"]), 0.5)
  expect_equal(as.numeric(w$weights["s1", "c2"]), 0.5)
})

test_that("calc_allocation_weights applies population weighting", {
  pts_src <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  src <- sf::st_sf(id = "s1", geometry = pts_src)

  pts_snk <- sf::st_sfc(
    sf::st_point(c(0, 1)),
    sf::st_point(c(0, 1)),
    crs = 3857
  )
  snk <- sf::st_sf(id = c("c1", "c2"), geometry = pts_snk)

  dmat <- calc_distance_matrix(src, snk)
  pops <- data.frame(id = c("c1", "c2"), population = c(100, 300))

  w <- calc_allocation_weights(
    dmat,
    sink_pop = pops,
    kernel_options = kernel_options("uniform")
  )

  expect_equal(as.numeric(w$weights["s1", "c1"]), 0.25)
  expect_equal(as.numeric(w$weights["s1", "c2"]), 0.75)
})

test_that("calc_allocation_weights supports multiple kernel options", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 0)),
    crs = 3857
  )
  src <- sf::st_sf(id = c("pub_1", "priv_1"), geometry = pts_src)

  pts_snk <- sf::st_sfc(
    sf::st_point(c(0, 1)),
    sf::st_point(c(0, 2)),
    crs = 3857
  )
  snk <- sf::st_sf(id = c("c1", "c2"), geometry = pts_snk)

  dmat <- calc_distance_matrix(src, snk)
  k_opts <- list(
    pub = kernel_options("uniform"),
    priv = kernel_options("gravity", list(beta = 1))
  )

  w <- calc_allocation_weights(dmat, kernel_options = k_opts)
  expect_equal(as.numeric(w$weights["pub_1", "c1"]), 0.5)
  expect_equal(as.numeric(w$weights["pub_1", "c2"]), 0.5)
  expect_gt(
    as.numeric(w$weights["priv_1", "c1"]),
    as.numeric(w$weights["priv_1", "c2"])
  )
})
