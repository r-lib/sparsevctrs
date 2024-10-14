test_that("sparse_sd() works", {
  x <- sparse_double(10, 5, 1000)

  expect_equal(sd(x), sparse_sd(x))

  x <- sparse_double(c(10, -10), c(5, 100), 1000)

  expect_equal(sd(x), sparse_sd(x))

  x <- sparse_double(c(10, -10), c(5, 100), 1000, default = 20)

  expect_equal(sd(x), sparse_sd(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(sd(x), sparse_sd(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(sd(x), sparse_sd(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(sd(x, na.rm = TRUE), sparse_sd(x, na_rm = TRUE))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_equal(sd(x, na.rm = TRUE), sparse_sd(x, na_rm = TRUE))

  x <- sparse_double(numeric(), integer(), 1000)

  expect_equal(sd(x), sparse_sd(x))

  x <- sparse_double(numeric(), integer(), 1000, default = 100)

  expect_equal(sd(x), sparse_sd(x))
})
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
