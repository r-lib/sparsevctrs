test_that("sparse_sqrt() works", {
  x <- sparse_integer(10, 5, 1000)

  expect_true(is_sparse_double(sparse_sqrt(x)))
  expect_equal(sqrt(x), sparse_sqrt(x))

  x <- sparse_double(c(10, 100), c(5, 100), 1000)

  expect_true(is_sparse_double(sparse_sqrt(x)))
  expect_equal(sqrt(x), sparse_sqrt(x))

  x <- sparse_double(c(10, 100), c(5, 100), 1000, default = 20)

  expect_true(is_sparse_double(sparse_sqrt(x)))
  expect_equal(sqrt(x), sparse_sqrt(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_true(is_sparse_double(sparse_sqrt(x)))
  expect_equal(sqrt(x), sparse_sqrt(x))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000, default = 100)

  expect_true(is_sparse_double(sparse_sqrt(x)))
  expect_equal(sqrt(x), sparse_sqrt(x))

  x <- sparse_double(numeric(), integer(), 1000)

  expect_true(is_sparse_double(sparse_sqrt(x)))
  expect_equal(sqrt(x), sparse_sqrt(x))

  x <- sparse_double(numeric(), integer(), 1000, default = 100)

  expect_true(is_sparse_double(sparse_sqrt(x)))
  expect_equal(sqrt(x), sparse_sqrt(x))
})
