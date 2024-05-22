test_that("coerce_to_sparse_matrix() works", {
  skip_if_not_installed("Matrix")

  sparse_df <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
  names(sparse_df) <- letters[1:10]
  sparse_df <- as.data.frame(sparse_df)

  res <- coerce_to_sparse_matrix(sparse_df)
  expect_s4_class(res, "dgCMatrix")
  expect_identical(dim(res), c(10L, 10L))

  exp <- Matrix::diag(1:10, 10, 10)
  exp <- Matrix::Matrix(exp, sparse = TRUE)
  exp <- as(exp, "generalMatrix")
  exp <- as(exp, "CsparseMatrix")
  colnames(exp) <- colnames(res)
  rownames(exp) <- rownames(res)

  expect_identical(res, exp)
})

test_that("coerce_to_sparse_matrix() errors on wrong input", {
  skip_if_not_installed("Matrix")

  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_matrix(1:10)
  )
  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_matrix(matrix(0, nrow = 10, ncol = 10))
  )
  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_matrix(iris)
  )
})

test_that("coerce_to_sparse_matrix() will divert for non-sparse data.frames", {
  skip_if_not_installed("Matrix")

  expect_identical(
    coerce_to_sparse_matrix(mtcars),
    Matrix::Matrix(as.matrix(mtcars), sparse = TRUE)
  )
})

test_that("coerce_to_sparse_matrix() materializes non-zero defaulted columns", {
  skip_if_not_installed("Matrix")
  withr::local_options("sparsevctrs.verbose_materialize" = TRUE)

  sparse_df <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
  names(sparse_df) <- letters[1:10]
  sparse_df <- as.data.frame(sparse_df)
  sparse_df$nonzero1 <- sparse_double(1, 1, 10, default = 10)
  sparse_df$nonzero2 <- sparse_double(1, 1, 10, default = 20)
  
  expect_snapshot(
    res <- coerce_to_sparse_matrix(sparse_df)
  )

  withr::local_options("sparsevctrs.verbose_materialize" = NULL)

  expect_s4_class(res, "dgCMatrix")
  expect_identical(dim(res), c(10L, 12L))

  exp <- Matrix::diag(1:10, 10, 10)
  exp <- Matrix::Matrix(exp, sparse = TRUE)
  exp <- as(exp, "generalMatrix")
  exp <- as(exp, "CsparseMatrix")

  exp <- cbind(exp, sparse_df$nonzero1)
  exp <- cbind(exp, sparse_df$nonzero2)

  colnames(exp) <- colnames(res)
  rownames(exp) <- rownames(res)

  expect_identical(res, exp)
})
