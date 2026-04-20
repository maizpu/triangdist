library(testthat)
library(triangdist)

test_that("dtriang returns correct value at known point", {
  expect_equal(
    dtriang(0.5, 0, 1, 0.7),
    1.428571,
    tolerance = 1e-6
  )
})

test_that("dtriang returns 0 outside bounds", {
  expect_equal(dtriang(-1, 0, 1, 0.7), 0)
  expect_equal(dtriang(2, 0, 1, 0.7), 0)
})

test_that("force exact mode branch", {
  mode <- 0.7

  # esto es clave: EXACT match sin decimales raros
  expect_equal(dtriang(mode, 0, 1, mode), 2)
  expect_equal(ptriang(mode, 0, 1, mode), ptriang(0.7, 0, 1, 0.7))
})

test_that("ptriang is between 0 and 1", {
  expect_true(ptriang(0.5, 0, 1, 0.7) >= 0)
  expect_true(ptriang(0.5, 0, 1, 0.7) <= 1)
})

test_that("ptriang at bounds", {
  expect_equal(ptriang(0, 0, 1, 0.7), 0)
  expect_equal(ptriang(1, 0, 1, 0.7), 1)
})

test_that("force exact p_mode branch", {
  p_mode <- ptriang(0.7, 0, 1, 0.7)

  # EXACT equality branch
  x <- qtriang(p_mode, 0, 1, 0.7)

  expect_true(abs(x - 0.7) < 1e-8)
})

test_that("qtriang returns numeric output", {
  expect_type(qtriang(0.5, 0, 1, 0.7), "double")
})

test_that("qtriang handles vector input", {
  expect_equal(length(qtriang(c(0.1, 0.5), 0, 1, 0.7)), 2)
})

test_that("qtriang is monotonic", {
  q <- qtriang(c(0.2, 0.8), 0, 1, 0.7)
  expect_true(q[1] <= q[2])
})


test_that("rtriang returns correct length", {
  expect_equal(length(rtriang(10, 0, 1, 0.7)), 10)
})

test_that("rtriang returns numeric", {
  expect_type(rtriang(5, 0, 1, 0.7), "double")
})

test_that("force rtriang execution properly", {
  set.seed(1)

  x <- rtriang(1000, 0, 1, 0.7)

  expect_true(all(x >= 0 & x <= 1))
  expect_equal(length(x), 1000)
})

test_that("errors are thrown correctly", {
  expect_error(dtriang(0.5, 1, 0, 0.7))
  expect_error(dtriang(0.5, 0, 1, 1.5))

  expect_error(ptriang(0.5, 1, 0, 0.7))
  expect_error(ptriang(0.5, 1, 2, 3))

  expect_error(qtriang(1, 1.1, 1, 0.7))
  expect_error(qtriang(0.1, 0, 0.5, 0.7))
  expect_error(qtriang(-0.1, 0, 1, 0.7))

})

test_that("edge cases behave correctly", {
  expect_equal(dtriang(0, 0, 1, 0.5), 0)
  expect_equal(ptriang(0, 0, 1, 0.5), 0)
})
