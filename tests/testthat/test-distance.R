test_that("calc_distance_matrix calculates pairwise distances correctly", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    crs = 3857
  )
  src <- sf::st_sf(id = c("s1", "s2"), geometry = pts_src)

  pts_snk <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 5)),
    crs = 3857
  )
  snk <- sf::st_sf(id = c("c1", "c2"), geometry = pts_snk)

  dmat <- calc_distance_matrix(src, snk)

  expect_s3_class(dmat, "imumap_dist_matrix")
  expect_equal(dim(dmat), c(2, 2))
  expect_equal(dmat$matrix["s1", "c1"], 0)
  expect_equal(dmat$matrix["s1", "c2"], 5)
  expect_equal(dmat$matrix["s2", "c1"], 10)
  expect_equal(dmat$matrix["s2", "c2"], 5)
})

test_that("calc_distance_matrix applies max_distance cutoff", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 100)),
    crs = 3857
  )
  src <- sf::st_sf(id = c("s1", "s2"), geometry = pts_src)

  pts_snk <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  snk <- sf::st_sf(id = "c1", geometry = pts_snk)

  dmat <- calc_distance_matrix(src, snk, max_distance = 50)
  expect_equal(dmat$matrix["s1", "c1"], 0)
  expect_true(is.infinite(dmat$matrix["s2", "c1"]))
})

test_that("calc_distance_matrix applies eligibility filtering operator", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    crs = 3857
  )
  src <- sf::st_sf(id = c("s1", "s2"), geometry = pts_src)

  pts_snk <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    crs = 3857
  )
  snk <- sf::st_sf(id = c("c1", "c2"), geometry = pts_snk)

  # Custom eligibility: s1 only to c1, s2 only to c2
  elig_func <- function(s, k) {
    outer(s$id, k$id, function(sid, kid) {
      (sid == "s1" & kid == "c1") | (sid == "s2" & kid == "c2")
    })
  }

  dmat <- calc_distance_matrix(src, snk, eligibility = elig_func)
  expect_equal(dmat$matrix["s1", "c1"], 0)
  expect_true(is.infinite(dmat$matrix["s1", "c2"]))
  expect_true(is.infinite(dmat$matrix["s2", "c1"]))
  expect_equal(dmat$matrix["s2", "c2"], 0)
})

test_that("print.imumap_dist_matrix outputs informative summary", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  src <- sf::st_sf(id = "s1", geometry = pts)
  snk <- sf::st_sf(id = "c1", geometry = pts)
  dmat <- calc_distance_matrix(src, snk)

  out <- capture.output(print(dmat))
  expect_true(any(grepl("<imumap_dist_matrix>", out)))
  expect_true(any(grepl("Sources: 1", out)))
})
