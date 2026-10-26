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
