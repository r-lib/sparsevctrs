### coerce_to_sparse_matrix ----------------------------------------------------
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

test_that("coerce_to_sparse_matrix() with zero rows and columns", {
  skip_if_not_installed("Matrix")

  dat <- data.frame()
  exp <- Matrix::Matrix(nrow = 0, ncol = 0, sparse = TRUE)
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(x = integer(), y = integer())
  exp <- Matrix::Matrix(nrow = 0, ncol = 2, sparse = TRUE)
  colnames(exp) <- c("x", "y")

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )


  dat <- data.frame(x = 1:2)[, integer()]
  exp <- Matrix::Matrix(nrow = 2, ncol = 0, sparse = TRUE)
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )
})

test_that("coerce_to_sparse_matrix() works with single all sparse vector", {
  skip_if_not_installed("Matrix")

  exp <- Matrix::Matrix(0, nrow = 10, ncol = 1, sparse = TRUE)
  colnames(exp) <- c("x")

  dat <- data.frame(x = rep(0, 10))
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(x = sparse_integer(integer(), integer(), 10))

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )
})

test_that("coerce_to_sparse_matrix() works with multiple all sparse vector", {
  skip_if_not_installed("Matrix")

  exp <- Matrix::Matrix(0, nrow = 10, ncol = 2, sparse = TRUE)
  colnames(exp) <- c("x", "y")

  dat <- data.frame(x = rep(0, 10), y = rep(0, 10))
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(integer(), integer(), 10),
    y = sparse_integer(integer(), integer(), 10)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )
})

test_that("coerce_to_sparse_matrix() works with sparse between dense", {
  skip_if_not_installed("Matrix")

  exp <- Matrix::Matrix(c(1, 0, 0, 0, 0, 1), nrow = 2, ncol = 3, sparse = TRUE)
  colnames(exp) <- c("x", "y", "z")

  dat <- data.frame(
    x = c(1, 0), 
    y = c(0, 0), 
    z = c(0, 1)
  )
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(1, 1, 2), 
    y = c(0, 0), 
    z = c(0, 1)
  )
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = c(1, 0), 
    y = c(0, 0), 
    z = sparse_integer(1, 2, 2)
  )
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(1, 1, 2), 
    y = c(0, 0), 
    z = sparse_integer(1, 2, 2)
  )
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = c(1, 0), 
    y = sparse_integer(integer(), integer(), 2), 
    z = c(0, 1)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(1, 1, 2), 
    y = sparse_integer(integer(), integer(), 2), 
    z = c(0, 1)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = c(1, 0), 
    y = sparse_integer(integer(), integer(), 2), 
    z = sparse_integer(1, 2, 2)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(1, 1, 2), 
    y = sparse_integer(integer(), integer(), 2), 
    z = sparse_integer(1, 2, 2)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )
})

test_that("coerce_to_sparse_matrix() works with sparse before dense", {
  skip_if_not_installed("Matrix")

  exp <- Matrix::Matrix(c(0, 0, 0, 0, 0, 1), nrow = 3, ncol = 2, sparse = TRUE)
  colnames(exp) <- c("x", "y")

  dat <- data.frame(
    x = c(0, 0, 0), 
    y = c(0, 0, 1)
  )
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = c(0, 0, 0), 
    y = sparse_integer(1, 3, 3)
  )
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(integer(), integer(), 3), 
    y = c(0, 0, 1)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(integer(), integer(), 3), 
    y = sparse_integer(1, 3, 3)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )
})

test_that("coerce_to_sparse_matrix() works with sparse after dense", {
  skip_if_not_installed("Matrix")

  exp <- Matrix::Matrix(c(1, 0, 0, 0, 0, 0), nrow = 3, ncol = 2, sparse = TRUE)
  colnames(exp) <- c("x", "y")

  dat <- data.frame(
    x = c(1, 0, 0), 
    y = c(0, 0, 0)
  )
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(1, 1, 3),
    y = c(0, 0, 0)
  )
  
  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = c(1, 0, 0),
    y = sparse_integer(integer(), integer(), 3)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )

  dat <- data.frame(
    x = sparse_integer(1, 1, 3),
    y = sparse_integer(integer(), integer(), 3)
  )

  expect_identical(
    coerce_to_sparse_matrix(dat),
    exp
  )
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

test_that("coerce_to_sparse_matrix() can pass through error call", {
  tmp_fun <- function(x) {
    coerce_to_sparse_matrix(x, call = rlang::caller_env(0))
  }
  
  expect_snapshot(
    error = TRUE,
    tmp_fun(1)
  )
})

### coerce_to_sparse_data_frame ------------------------------------------------

test_that("coerce_to_sparse_data_frame() works", {
  skip_if_not_installed("Matrix")

  sparse_mat <- Matrix::diag(1:10, 10, 10)
  sparse_mat <- Matrix::Matrix(sparse_mat, sparse = TRUE)
  sparse_mat <- as(sparse_mat, "generalMatrix")
  sparse_mat <- as(sparse_mat, "CsparseMatrix")
  colnames(sparse_mat) <- letters[1:10]
  rownames(sparse_mat) <- 1:10

  res <- coerce_to_sparse_data_frame(sparse_mat)

  exp <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
  names(exp) <- letters[1:10]
  exp <- as.data.frame(exp)

  expect_identical(res, exp)
})

test_that("coerce_to_sparse_data_frame() works with non-dgCMatrix input", {
  skip_if_not_installed("Matrix")

  sparse_mat <- Matrix::diag(1:10, 10, 10)
  sparse_mat <- Matrix::Matrix(sparse_mat, sparse = TRUE)
  colnames(sparse_mat) <- letters[1:10]
  rownames(sparse_mat) <- 1:10

  res <- coerce_to_sparse_data_frame(sparse_mat)

  exp <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
  names(exp) <- letters[1:10]
  exp <- as.data.frame(exp)

  expect_identical(res, exp)
})

test_that("coerce_to_sparse_data_frame() errors with no column names", {
  skip_if_not_installed("Matrix")

  sparse_mat <- Matrix::diag(1:10, 10, 10)
  sparse_mat <- Matrix::Matrix(sparse_mat, sparse = TRUE)

  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_data_frame(sparse_mat)
  )
})

test_that("coerce_to_sparse_data_frame() errors with wrong input", {
  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_data_frame(mtcars)
  )
  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_data_frame(1:10)
  )
})

test_that("coerce_to_sparse_data_frame() can pass through error call", {
  tmp_fun <- function(x) {
    coerce_to_sparse_data_frame(x, call = rlang::caller_env(0))
  }
  
  expect_snapshot(
    error = TRUE,
    tmp_fun(1)
  )
})

### coerce_to_sparse_tibble ----------------------------------------------------

test_that("coerce_to_sparse_tibble() works", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("tibble")

  sparse_mat <- Matrix::diag(1:10, 10, 10)
  sparse_mat <- Matrix::Matrix(sparse_mat, sparse = TRUE)
  sparse_mat <- as(sparse_mat, "generalMatrix")
  sparse_mat <- as(sparse_mat, "CsparseMatrix")
  colnames(sparse_mat) <- letters[1:10]
  rownames(sparse_mat) <- 1:10

  res <- coerce_to_sparse_tibble(sparse_mat)

  exp <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
  names(exp) <- letters[1:10]
  exp <- tibble::as_tibble(exp)

  expect_identical(res, exp)
})

test_that("coerce_to_sparse_tibble() works with non-dgCMatrix input", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("tibble")

  sparse_mat <- Matrix::diag(1:10, 10, 10)
  sparse_mat <- Matrix::Matrix(sparse_mat, sparse = TRUE)
  colnames(sparse_mat) <- letters[1:10]
  rownames(sparse_mat) <- 1:10

  res <- coerce_to_sparse_tibble(sparse_mat)

  exp <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
  names(exp) <- letters[1:10]
  exp <- tibble::as_tibble(exp)

  expect_identical(res, exp)
})

test_that("coerce_to_sparse_tibble() errors with no column names", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("tibble")

  sparse_mat <- Matrix::diag(1:10, 10, 10)
  sparse_mat <- Matrix::Matrix(sparse_mat, sparse = TRUE)

  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_tibble(sparse_mat)
  )
})

test_that("coerce_to_sparse_tibble() errors with wrong input", {
  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_tibble(mtcars)
  )
  expect_snapshot(
    error = TRUE,
    coerce_to_sparse_tibble(1:10)
  )
})

test_that("coerce_to_sparse_matrix() can pass through error call", {
  tmp_fun <- function(x) {
    coerce_to_sparse_tibble(x, call = rlang::caller_env(0))
  }
  
  expect_snapshot(
    error = TRUE,
    tmp_fun(1)
  )
})

### .sparse_matrix_to_list -----------------------------------------------------

test_that(".sparse_matrix_to_list() handles fully sparse columns (#69)", {
  skip_if_not_installed("Matrix")
  
  x_mat <- matrix(
    c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), 
    nrow = 3
  )
  colnames(x_mat) <- letters[1:6]
  
  x_df <- as.data.frame(x_mat)
  x_mat_sparse <- Matrix::Matrix(x_mat, sparse = TRUE)

  expect_identical(
    coerce_to_sparse_data_frame(x_mat_sparse),
    x_df
  )
})
