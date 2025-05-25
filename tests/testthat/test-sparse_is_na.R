test_that("sparse_is_na() works - double", {
  x <- sparse_double(c(10, -10), c(5, 100), 1000)

  expect_equal(is.na(x), sparse_is_na(x))
  expect_equal(as.integer(is.na(x)), sparse_is_na(x, type = "integer"))

  x <- sparse_double(c(NA, 10, 30), 1:3, 1000)

  expect_equal(is.na(x), sparse_is_na(x))
  expect_equal(as.integer(is.na(x)), sparse_is_na(x, type = "integer"))
})

test_that("sparse_is_na() works - integer", {
  x <- sparse_integer(c(10, -10), c(5, 100), 1000)

  expect_equal(is.na(x), sparse_is_na(x))
  expect_equal(as.integer(is.na(x)), sparse_is_na(x, type = "integer"))

  x <- sparse_integer(c(NA, 10, 30), 1:3, 1000)

  expect_equal(is.na(x), sparse_is_na(x))
  expect_equal(as.integer(is.na(x)), sparse_is_na(x, type = "integer"))
})

test_that("sparse_is_na() works - logical", {
  x <- sparse_logical(c(TRUE, TRUE), c(5, 100), 1000)

  expect_equal(is.na(x), sparse_is_na(x))
  expect_equal(as.integer(is.na(x)), sparse_is_na(x, type = "integer"))

  x <- sparse_logical(c(NA, TRUE, TRUE), 1:3, 1000)

  expect_equal(is.na(x), sparse_is_na(x))
  expect_equal(as.integer(is.na(x)), sparse_is_na(x, type = "integer"))
})

test_that("sparse_is_na() works - character", {
  x <- sparse_character(c("A", "B"), c(5, 100), 1000)

  expect_equal(is.na(x), sparse_is_na(x))
  expect_equal(as.integer(is.na(x)), sparse_is_na(x, type = "integer"))

  x <- sparse_character(c(NA, "A", "B"), 1:3, 1000)

  expect_equal(is.na(x), sparse_is_na(x))
  expect_equal(as.integer(is.na(x)), sparse_is_na(x, type = "integer"))
})
