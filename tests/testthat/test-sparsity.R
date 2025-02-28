test_that("works with data.frames", {
  mtcars_exp_sparsity <- mean(mtcars == 0)

  expect_identical(
    sparsity(mtcars),
    mtcars_exp_sparsity
  )
})

test_that("works with non-numeric data.frames", {
  vs <- mtcars$vs
  mtcars$vs <- 4
  mtcars_exp_sparsity <- mean(mtcars == 0)

  mtcars$vs <- as.character(vs)

  expect_identical(
    sparsity(mtcars),
    mtcars_exp_sparsity
  )

  mtcars$vs <- as.logical(vs)

  expect_identical(
    sparsity(mtcars),
    mtcars_exp_sparsity
  )

  mtcars$vs <- ifelse(vs == 1, 1, NA)

  expect_identical(
    sparsity(mtcars),
    mtcars_exp_sparsity
  )
})

test_that("works with data.frames sample arg", {
  set.seed(1234)
  exp <- mean(mtcars[sample(32, 10), ] == 0)

  set.seed(1234)
  expect_identical(
    sparsity(mtcars, sample = 10),
    exp
  )

  set.seed(1234)
  exp <- mean(mtcars == 0)

  set.seed(1234)
  expect_identical(
    sparsity(mtcars, sample = 1000),
    exp
  )

  expect_snapshot(
    error = TRUE,
    sparsity(mtcars, sample = 0.4)
  )
})

test_that("works with matrices", {
  mtcars_mat <- as.matrix(mtcars)
  mtcars_exp_sparsity <- mean(mtcars_mat == 0)

  expect_identical(
    sparsity(mtcars_mat),
    mtcars_exp_sparsity
  )

  mtcars_mat[1, 1] <- NA

  expect_identical(
    sparsity(mtcars_mat),
    mtcars_exp_sparsity
  )

  mtcars_lgl <- apply(mtcars_mat, 2, as.logical)

  expect_identical(
    sparsity(mtcars_lgl),
    0
  )

  mtcars_chr <- apply(mtcars_mat, 2, as.character)

  expect_identical(
    sparsity(mtcars_chr),
    0
  )
})

test_that("works with sparse matrices", {
  mtcars_sparse_mat <- coerce_to_sparse_matrix(mtcars)
  mtcars_exp_sparsity <- mean(as.logical(mtcars_sparse_mat == 0))

  expect_equal(
    sparsity(mtcars_sparse_mat),
    mtcars_exp_sparsity
  )

  mtcars_sparse_mat[1, 1] <- NA

  expect_equal(
    sparsity(mtcars_sparse_mat),
    mtcars_exp_sparsity
  )
})
