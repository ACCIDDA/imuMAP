test_that("canonicalize_spatial validates spatial sf objects", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1)),
    crs = 4326
  )
  src <- sf::st_sf(id = c("s1", "s2"), geometry = pts_src)

  poly_tgt <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0),
      ncol = 2,
      byrow = TRUE
    ))),
    crs = 4326
  )
  tgt <- sf::st_sf(id = "c1", geometry = poly_tgt)

  res <- canonicalize_spatial(src, tgt)
  expect_s3_class(res, "imumap_spatial")
  expect_equal(res$source$id, c("s1", "s2"))
  expect_equal(res$target$id, "c1")
  expect_equal(sf::st_crs(res$source), sf::st_crs(res$target))
})

test_that("canonicalize_spatial reprojects source to match target CRS", {
  pts_src <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  src <- sf::st_sf(id = "s1", geometry = pts_src)

  pts_tgt <- sf::st_sfc(sf::st_point(c(1000, 1000)), crs = 3857)
  tgt <- sf::st_sf(id = "c1", geometry = pts_tgt)

  res <- canonicalize_spatial(src, tgt)
  expect_equal(sf::st_crs(res$source), sf::st_crs(tgt))
})

test_that("canonicalize_spatial errors on missing sf class or columns", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  src_no_id <- sf::st_sf(name = "school1", geometry = pts)
  tgt <- sf::st_sf(id = "c1", geometry = pts)

  expect_error(
    canonicalize_spatial(data.frame(id = "s1"), tgt),
    "must be an 'sf' object"
  )
  expect_error(
    canonicalize_spatial(src_no_id, tgt),
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
  tgt <- sf::st_sf(
    id = "c1",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )

  expect_error(
    canonicalize_spatial(src_dup, tgt),
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
    "target demographic population must be numeric and non-negative"
  )
})
