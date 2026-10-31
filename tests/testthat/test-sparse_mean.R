test_that("sparse_mean() works", {
  x <- sparse_double(10, 5, 1000)

  expect_equal(mean(x), sparse_mean(x))

  x <- sparse_double(c(10, -10), c(5, 100), 1000)

  expect_equal(mean(x), sparse_mean(x))

  x <- sparse_double(c(10, -10), c(5, 100), 1000, default = 20)

  expect_equal(mean(x), sparse_mean(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(mean(x), sparse_mean(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(mean(x), sparse_mean(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(mean(x, na.rm = TRUE), sparse_mean(x, na_rm = TRUE))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(mean(x, na.rm = TRUE), sparse_mean(x, na_rm = TRUE))

  x <- sparse_double(numeric(), integer(), 1000)

  expect_equal(mean(x), sparse_mean(x))

  x <- sparse_double(numeric(), integer(), 1000, default = 100)

  expect_equal(mean(x), sparse_mean(x))
})

test_that("sparse_mean() works with wts argument", {
  x <- sparse_double(10, 5, 1000)
  wts <- (1:1000)[]

  expect_equal(weighted.mean(x, wts), sparse_mean(x, wts = wts))

  x <- sparse_double(c(10, -10), c(5, 100), 1000)

  expect_equal(weighted.mean(x, wts), sparse_mean(x, wts = wts))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(weighted.mean(x, wts), sparse_mean(x, wts = wts))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(weighted.mean(x, wts), sparse_mean(x, wts = wts))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(
    weighted.mean(x, wts, na.rm = TRUE),
    sparse_mean(x, wts = wts, na_rm = TRUE)
  )

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(
    weighted.mean(x, wts, na.rm = TRUE),
    sparse_mean(x, wts = wts, na_rm = TRUE)
  )

  x <- sparse_double(numeric(), integer(), 1000)

  expect_equal(weighted.mean(x, wts), sparse_mean(x, wts = wts))

  x <- sparse_double(numeric(), integer(), 1000, default = 100)

  expect_equal(weighted.mean(x, wts), sparse_mean(x, wts = wts))
})

test_that("sparse_mean() does not overflow with integer default", {
  x <- sparse_integer(1L, 1, 1e9, default = 5L)

  expect_no_warning(res <- sparse_mean(x))
  expect_equal(res, (1 + 5 * (1e9 - 1)) / 1e9)
})

test_that("sparse_mean() divides by zero-sum weights", {
  x <- sparse_double(c(1, 2), 1:2, 6L)
  wts <- rep(0, 6)

  expect_equal(sparse_mean(x, wts = wts), weighted.mean(x, wts))

  x <- sparse_double(numeric(), integer(), 6L)

  expect_equal(sparse_mean(x, wts = wts), weighted.mean(x, wts))
})

test_that("sparse_mean() works with wts and NA values at specific positions", {
  x <- sparse_double(c(1, NA, 3), c(2, 5, 9), 10)
  wts <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  expect_equal(weighted.mean(x, wts), sparse_mean(x, wts = wts))
  expect_equal(
    weighted.mean(x, wts, na.rm = TRUE),
    sparse_mean(x, wts = wts, na_rm = TRUE)
  )

  wts[c(2, 7)] <- NA

  expect_equal(weighted.mean(x, wts), sparse_mean(x, wts = wts))
})
