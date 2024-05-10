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
