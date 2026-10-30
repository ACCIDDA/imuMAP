test_that("allocate calculates target counts and coverage rates accurately", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    crs = 3857
  )
  src <- sf::st_sf(id = c("s1", "s2"), geometry = pts_src)

  pts_tgt <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    crs = 3857
  )
  tgt <- sf::st_sf(id = c("c1", "c2"), geometry = pts_tgt)

  # Identity distance/weights
  dmat <- calc_distance_matrix(src, tgt, max_distance = 1)
  w <- calc_allocation_weights(dmat, kernel_options = kernel_options("uniform"))

  cov_data <- data.frame(
    id = c("s1", "s2"),
    vaccinated = c(90, 80),
    total = c(100, 100)
  )

  res <- allocate(cov_data, w)
  expect_s3_class(res, "data.table")
  expect_equal(res$id, c("c1", "c2"))
  expect_equal(res$vaccinated, c(90, 80))
  expect_equal(res$total, c(100, 100))
  expect_equal(res$coverage, c(0.9, 0.8))
})

test_that("allocate handles posterior draws correctly", {
  pts_src <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  src <- sf::st_sf(id = "s1", geometry = pts_src)

  pts_tgt <- sf::st_sfc(
    sf::st_point(c(0, 1)),
    sf::st_point(c(0, 1)),
    crs = 3857
  )
  tgt <- sf::st_sf(id = c("c1", "c2"), geometry = pts_tgt)

  dmat <- calc_distance_matrix(src, tgt)
  w <- calc_allocation_weights(dmat, kernel_options = kernel_options("uniform"))

  draws_df <- data.frame(
    id = c("s1", "s1", "s1"),
    draw = c(1, 2, 3),
    coverage = c(0.8, 0.85, 0.9)
  )

  res_draws <- allocate(draws_df, w, draws_col = "draw")
  expect_equal(nrow(res_draws), 6) # 2 targets * 3 draws
  expect_true(all(c("id", "draw", "value") %in% names(res_draws)))
  expect_equal(res_draws[draw == 1]$value, c(0.4, 0.4))
})

test_that("allocate validates weight object and column names", {
  expect_error(
    allocate(data.frame(id = "s1", coverage = 0.5), list()),
    "argument `weights` must be an 'imumap_weights' object"
  )
})
