test_that("kernel_options instantiates all standard kernels", {
  grav <- kernel_options("gravity", list(beta = 2))
  expect_s3_class(grav, "imumap_kernel_options")
  expect_equal(grav$kernel, "gravity")
  expect_equal(grav$params$beta, 2)
  expect_equal(grav$params$min_dist, 1)

  expo <- kernel_options("exponential", list(decay_rate = 0.1))
  expect_equal(expo$kernel, "exponential")
  expect_equal(expo$params$decay_rate, 0.1)

  unif <- kernel_options("uniform")
  expect_equal(unif$kernel, "uniform")

  gauss <- kernel_options("gaussian", list(sigma = 500))
  expect_equal(gauss$kernel, "gaussian")
  expect_equal(gauss$params$sigma, 500)
})

test_that("apply_kernel computes decay values accurately", {
  d <- matrix(c(1, 2, Inf, 0), nrow = 2, byrow = TRUE)

  # Gravity
  grav_opt <- kernel_options("gravity", list(beta = 2, min_dist = 1))
  w_grav <- imuMAP:::apply_kernel(d, grav_opt)
  expect_equal(w_grav[1, 1], 1 / (1^2))
  expect_equal(w_grav[1, 2], 1 / (2^2))
  expect_equal(w_grav[2, 1], 0)
  expect_equal(w_grav[2, 2], 1 / (1^2)) # min_dist applied

  # Uniform
  unif_opt <- kernel_options("uniform")
  w_unif <- imuMAP:::apply_kernel(d, unif_opt)
  expect_equal(w_unif[1, 1], 1)
  expect_equal(w_unif[1, 2], 1)
  expect_equal(w_unif[2, 1], 0)
  expect_equal(w_unif[2, 2], 1)

  # Custom function
  custom_opt <- kernel_options(
    kernel = function(d, p) p$scale / (1 + d),
    params = list(scale = 10)
  )
  w_cust <- imuMAP:::apply_kernel(d, custom_opt)
  expect_equal(w_cust[1, 1], 10 / (1 + 1))
  expect_equal(w_cust[2, 1], 0)
})

test_that("print.imumap_kernel_options prints summary correctly", {
  opt <- kernel_options("gravity", list(beta = 2))
  out <- capture.output(print(opt))
  expect_true(any(grepl("<imumap_kernel_options>", out)))
  expect_true(any(grepl("Kernel: gravity", out)))
})
