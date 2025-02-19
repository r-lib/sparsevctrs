test_that("sparse_replace() works - integers", {
  x <- sparse_integer(c(NA, 10, 30), 1:3, 1000)

  res <- sparse_replace_na(x, 100L)
  exp <- x
  exp[is.na(exp)] <- 100L

  expect_identical(res, exp)
  expect_true(is_sparse_integer(res))

  # replace == default
  res <- sparse_replace_na(x, 0L)
  exp <- x
  exp[is.na(exp)] <- 0L

  expect_identical(res, exp)
  expect_true(is_sparse_integer(res))
})

test_that("sparse_replace() works - doubles", {
  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  res <- sparse_replace_na(x, 100)
  exp <- x
  exp[is.na(exp)] <- 100

  expect_identical(res, exp)
  expect_true(is_sparse_double(res))

  # replace == default
  res <- sparse_replace_na(x, 0)
  exp <- x
  exp[is.na(exp)] <- 0

  expect_identical(res, exp)
  expect_true(is_sparse_double(res))
})

test_that("sparse_replace() works - characters", {
  x <- sparse_character(c(NA, "A", "B"), 1:3, 1000)

  res <- sparse_replace_na(x, "M")
  exp <- x
  exp[is.na(exp)] <- "M"

  expect_identical(res, exp)
  expect_true(is_sparse_character(res))

  # replace == default
  res <- sparse_replace_na(x, 0)
  exp <- x
  exp[is.na(exp)] <- 0

  expect_identical(res, exp)
  expect_true(is_sparse_character(res))
})
