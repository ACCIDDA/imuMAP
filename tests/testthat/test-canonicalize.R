test_that("canonicalize_spatial validates spatial sf objects", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1)),
    crs = 4326
  )
  src <- sf::st_sf(id = c("s1", "s2"), geometry = pts_src)

  poly_snk <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0),
      ncol = 2,
      byrow = TRUE
    ))),
    crs = 4326
  )
  snk <- sf::st_sf(id = "c1", geometry = poly_snk)

  res <- canonicalize_spatial(src, snk)
  expect_s3_class(res, "imumap_spatial")
  expect_equal(res$source$id, c("s1", "s2"))
  expect_equal(res$sink$id, "c1")
  expect_equal(sf::st_crs(res$source), sf::st_crs(res$sink))
})

test_that("canonicalize_spatial reprojects source to match sink CRS", {
  pts_src <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  src <- sf::st_sf(id = "s1", geometry = pts_src)

  pts_snk <- sf::st_sfc(sf::st_point(c(1000, 1000)), crs = 3857)
  snk <- sf::st_sf(id = "c1", geometry = pts_snk)

  res <- canonicalize_spatial(src, snk)
  expect_equal(sf::st_crs(res$source), sf::st_crs(snk))
})

test_that("canonicalize_spatial errors on missing sf class or columns", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  src_no_id <- sf::st_sf(name = "school1", geometry = pts)
  snk <- sf::st_sf(id = "c1", geometry = pts)

  expect_error(
    canonicalize_spatial(data.frame(id = "s1"), snk),
    "must be an 'sf' object"
  )
  expect_error(
    canonicalize_spatial(src_no_id, snk),
    "expected column 'id'"
  )
})

test_that("canonicalize_spatial errors on duplicate or empty IDs", {
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1)),
    crs = 4326
  )
  src_dup <- sf::st_sf(id = c("s1", "s1"), geometry = pts)
  snk <- sf::st_sf(
    id = "c1",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )

  expect_error(
    canonicalize_spatial(src_dup, snk),
    "duplicate IDs found"
  )
})

test_that("canonicalize_demographics validates demographic data frames", {
  df <- data.frame(id = c("c1", "c2"), population = c(100, 250))
  dt <- canonicalize_demographics(df)

  expect_s3_class(dt, "imumap_demographics")
  expect_equal(dt$population, c(100, 250))
  expect_equal(dt$id, c("c1", "c2"))
})

test_that("canonicalize_demographics rejects invalid columns or values", {
  expect_error(
    canonicalize_demographics(data.frame(cbg = "c1", population = 10)),
    "expected column 'id'"
  )
  expect_error(
    canonicalize_demographics(data.frame(id = "c1", pop = 10)),
    "expected column 'population'"
  )
  expect_error(
    canonicalize_demographics(data.frame(id = "c1", population = -5)),
    "sink demographic population must be numeric and non-negative"
  )
})
