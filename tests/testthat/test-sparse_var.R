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

test_that("sparse_var() and sparse_sd() work with dense input", {
  x <- c(1, 2, 3, 4)

  expect_equal(sparse_var(x), var(x))
  expect_equal(sparse_sd(x), sd(x))

  x <- c(1, NA, 3, 6)

  expect_equal(sparse_var(x), var(x))
  expect_equal(sparse_var(x, na_rm = TRUE), var(x, na.rm = TRUE))
})

test_that("sparse_var() returns NA for fewer than 2 observations", {
  expect_identical(
    sparse_var(sparse_double(numeric(), integer(), 0L)),
    NA_real_
  )
  expect_identical(
    sparse_var(sparse_double(numeric(), integer(), 1L)),
    NA_real_
  )
  expect_identical(sparse_var(sparse_double(1, 1, 1L)), NA_real_)
  expect_identical(sparse_var(c(1, NA), na_rm = TRUE), NA_real_)
})
