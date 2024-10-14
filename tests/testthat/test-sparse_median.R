test_that("sparse_median() works", {
  x <- sparse_double(10, 5, 1000)

  expect_equal(median(x), sparse_median(x))

  x <- sparse_double(c(10, -10), c(5, 100), 1000)

  expect_equal(median(x), sparse_median(x))

  x <- sparse_double(c(10, -10), c(5, 100), 1000, default = 20)

  expect_equal(median(x), sparse_median(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(median(x), sparse_median(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(median(x), sparse_median(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(median(x, na.rm = TRUE), sparse_median(x, na_rm = TRUE))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(median(x, na.rm = TRUE), sparse_median(x, na_rm = TRUE))

  x <- sparse_double(numeric(), integer(), 1000)

  expect_equal(median(x), sparse_median(x))

  x <- sparse_double(numeric(), integer(), 1000, default = 100)

  expect_equal(median(x), sparse_median(x))
})

test_that("sparse_median() edge cases", {
  x <- sparse_double(c(10, 10), c(1, 2), 4)

  expect_equal(median(x), sparse_median(x))

  x <- sparse_double(c(10, NA), c(1, 2), 4)

  expect_equal(median(x), sparse_median(x))
  expect_equal(median(x, na.rm = TRUE), sparse_median(x, na_rm = TRUE))

  x <- sparse_double(c(10, 10, NA), c(1, 2, 3), 5)

  expect_equal(median(x), sparse_median(x))
  expect_equal(median(x, na.rm = TRUE), sparse_median(x, na_rm = TRUE))
})
