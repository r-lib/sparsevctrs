test_that("sparse_var() works", {
  x <- sparse_double(10, 5, 1000)

  expect_equal(var(x), sparse_var(x))

  x <- sparse_double(c(10, -10), c(5, 100), 1000)

  expect_equal(var(x), sparse_var(x))

  x <- sparse_double(c(10, -10), c(5, 100), 1000, default = 20)

  expect_equal(var(x), sparse_var(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(var(x), sparse_var(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(var(x), sparse_var(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(var(x, na.rm = TRUE), sparse_var(x, na_rm = TRUE))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(var(x, na.rm = TRUE), sparse_var(x, na_rm = TRUE))

  x <- sparse_double(numeric(), integer(), 1000)

  expect_equal(var(x), sparse_var(x))

  x <- sparse_double(numeric(), integer(), 1000, default = 100)

  expect_equal(var(x), sparse_var(x))
})
