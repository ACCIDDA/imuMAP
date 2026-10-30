test_that("eligibility_rule filters by source metadata and match_cols", {
  pts_src <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    sf::st_point(c(0, 20)),
    crs = 3857
  )
  src <- sf::st_sf(
    id = c("pub_d1", "pub_d2", "priv_all"),
    school_type = c("public", "public", "private"),
    district_id = c("d1", "d2", "d1"),
    geometry = pts_src
  )

  pts_tgt <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    crs = 3857
  )
  tgt <- sf::st_sf(
    id = c("c_d1", "c_d2"),
    district_id = c("d1", "d2"),
    geometry = pts_tgt
  )

  # Rule: Public schools only to matching district; Private schools everywhere within 50
  rule_pub <- eligibility_rule(
    source_filter = list(school_type = "public"),
    match_cols = "district_id"
  )
  rule_priv <- eligibility_rule(
    source_filter = list(school_type = "private"),
    max_distance = 50
  )

  dmat <- calc_distance_matrix(
    src,
    tgt,
    eligibility = eligibility_rules(rule_pub, rule_priv)
  )

  # pub_d1 can only reach c_d1
  expect_equal(dmat$matrix["pub_d1", "c_d1"], 0)
  expect_true(is.infinite(dmat$matrix["pub_d1", "c_d2"]))

  # pub_d2 can only reach c_d2
  expect_true(is.infinite(dmat$matrix["pub_d2", "c_d1"]))
  expect_equal(dmat$matrix["pub_d2", "c_d2"], 0)

  # priv_all can reach both c_d1 and c_d2 because distance is <= 50 and no match_cols
  expect_equal(dmat$matrix["priv_all", "c_d1"], 20)
  expect_equal(dmat$matrix["priv_all", "c_d2"], 10)
})

test_that("eligibility_rule handles custom predicates and target_filter", {
  pts_src <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  src <- sf::st_sf(id = "s1", grade = "elem", geometry = pts_src)

  pts_tgt <- sf::st_sfc(
    sf::st_point(c(0, 1)),
    sf::st_point(c(0, 2)),
    crs = 3857
  )
  tgt <- sf::st_sf(
    id = c("c1", "c2"),
    urban = c(TRUE, FALSE),
    geometry = pts_tgt
  )

  # Rule: only map to urban targets
  rule_urban <- eligibility_rule(target_filter = list(urban = TRUE))
  dmat <- calc_distance_matrix(src, tgt, eligibility = rule_urban)

  expect_equal(dmat$matrix["s1", "c1"], 1)
  expect_true(is.infinite(dmat$matrix["s1", "c2"]))
})

test_that("eligibility_rule errors when match_cols not found in metadata", {
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  src <- sf::st_sf(id = "s1", geometry = pts)
  tgt <- sf::st_sf(id = "c1", geometry = pts)

  rule <- eligibility_rule(match_cols = "nonexistent_col")
  expect_error(
    calc_distance_matrix(src, tgt, eligibility = rule),
    "expected match column 'nonexistent_col'"
  )
})

test_that("print.imumap_eligibility_rule prints summary accurately", {
  r <- eligibility_rule(
    list(type = "public"),
    match_cols = "district_id",
    max_distance = 1000
  )
  out <- capture.output(print(r))
  expect_true(any(grepl("<imumap_eligibility_rule>", out)))
  expect_true(any(grepl("district_id", out)))
})
