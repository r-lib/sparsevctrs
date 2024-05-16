test_that("as_sparse_double works", {
  x_dense <- c(3, 0, 2, 0, 0, 0, 4, 0, 0, NA)
  x_sparse <- as_sparse_double(x_dense)

  expect_true(is_sparse_double(x_sparse))
  expect_identical(x_sparse, x_dense)

  x_dense <- c(3L, 0L, 2L, 0L, 0L, 0L, 4L, 0L, 0L, NA)
  x_sparse <- as_sparse_double(x_dense)

  expect_true(is_sparse_double(x_sparse))
  expect_identical(x_sparse, as.numeric(x_dense))
})

test_that("as_sparse_integer works", {
  x_dense <- c(3, 0, 2, 0, 0, 0, 4, 0, 0, NA)
  x_sparse <- as_sparse_integer(x_dense)

  expect_true(is_sparse_integer(x_sparse))
  expect_identical(x_sparse, as.integer(x_dense))

  x_dense <- c(3L, 0L, 2L, 0L, 0L, 0L, 4L, 0L, 0L, NA)
  x_sparse <- as_sparse_integer(x_dense)

  expect_true(is_sparse_integer(x_sparse))
  expect_identical(x_sparse, x_dense)
})

test_that("as_sparse_integer works", {
  x_dense <- c("A", "", "B", "", "", "", "C", "", "", NA)
  x_sparse <- as_sparse_character(x_dense)

  expect_true(is_sparse_character(x_sparse))
})

test_that("as_sparse_logical works", {
  x_dense <- c(FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE, FALSE)
  x_sparse <- as_sparse_logical(x_dense)

  expect_true(is_sparse_logical(x_sparse))
})
