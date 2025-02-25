test_that("scalar multiplication works", {
  vec_int <- sparse_integer(c(4, 78), c(1, 10), 10)
  vec_double <- sparse_double(c(4, 78), c(1, 10), 10)

  exp_int <- as.integer(c(4, 0, 0, 0, 0, 0, 0, 0, 0, 78))
  exp_double <- c(4, 0, 0, 0, 0, 0, 0, 0, 0, 78)


  expect_identical(
    sparse_multiplication_scalar(vec_int, 0),
    exp_int * 0L
  )
  expect_identical(
    sparse_multiplication_scalar(vec_double, 0),
    exp_double * 0
  )
  expect_identical(
    sparse_multiplication_scalar(vec_int, 1),
    exp_int * 1L
  )
  expect_identical(
    sparse_multiplication_scalar(vec_double, 1),
    exp_double * 1
  )
  expect_identical(
    sparse_multiplication_scalar(vec_int, 5),
    exp_int * 5L
  )
  expect_identical(
    sparse_multiplication_scalar(vec_double, 5),
    exp_double * 5
  )

  expect_identical(
    sparse_multiplication_scalar(vec_double, 5.5623),
    exp_double * 5.5623
  )

  expect_true(
    is_sparse_integer(sparse_multiplication_scalar(vec_int, 0))
  )
  expect_true(
    is_sparse_double(sparse_multiplication_scalar(vec_double, 0))
  )
  expect_true(
    is_sparse_integer(sparse_multiplication_scalar(vec_int, 1))
  )
  expect_true(
    is_sparse_double(sparse_multiplication_scalar(vec_double, 1))
  )
  expect_true(
    is_sparse_integer(sparse_multiplication_scalar(vec_int, 5))
  )
  expect_true(
    is_sparse_double(sparse_multiplication_scalar(vec_double, 5))
  )
})

test_that("scalar division works", {
  vec_int <- sparse_integer(c(4, 78), c(1, 10), 10)
  vec_double <- sparse_double(c(4, 78), c(1, 10), 10)

  exp_double <- c(4, 0, 0, 0, 0, 0, 0, 0, 0, 78)

  expect_identical(
    sparse_division_scalar(vec_int, 0),
    rep(Inf, 10)
  )
  expect_identical(
    sparse_division_scalar(vec_double, 0),
    rep(Inf, 10)
  )
  expect_identical(
    sparse_division_scalar(vec_int, 1),
    as.integer(exp_double)
  )
  expect_identical(
    sparse_division_scalar(vec_double, 1),
    exp_double
  )
  expect_identical(
    sparse_division_scalar(vec_int, 2),
    exp_double / 2
  )
  expect_identical(
    sparse_division_scalar(vec_double, 5),
    exp_double / 5
  )

  expect_identical(
    sparse_division_scalar(vec_double, 5.5623),
    exp_double / 5.5623
  )

  expect_true(
    is_sparse_integer(sparse_division_scalar(vec_int, 1))
  )
  expect_true(
    is_sparse_double(sparse_division_scalar(vec_double, 1))
  )
  expect_true(
    is_sparse_double(sparse_division_scalar(vec_int, 5))
  )
  expect_true(
    is_sparse_double(sparse_division_scalar(vec_double, 5))
  )
})

test_that("scalar addition works", {
  vec_int <- sparse_integer(c(4, 78), c(1, 10), 10)
  vec_double <- sparse_double(c(4, 78), c(1, 10), 10)

  exp_int <- as.integer(c(4, 0, 0, 0, 0, 0, 0, 0, 0, 78))
  exp_double <- c(4, 0, 0, 0, 0, 0, 0, 0, 0, 78)


  expect_identical(
    sparse_addition_scalar(vec_int, 0),
    exp_int + 0L
  )
  expect_identical(
    sparse_addition_scalar(vec_double, 0),
    exp_double + 0
  )
  expect_identical(
    sparse_addition_scalar(vec_int, 1),
    exp_int + 1L
  )
  expect_identical(
    sparse_addition_scalar(vec_double, 1),
    exp_double + 1
  )
  expect_identical(
    sparse_addition_scalar(vec_int, 5),
    exp_int + 5L
  )
  expect_identical(
    sparse_addition_scalar(vec_double, 5),
    exp_double + 5
  )

  expect_identical(
    sparse_addition_scalar(vec_double, 5.5623),
    exp_double + 5.5623
  )

  expect_true(
    is_sparse_integer(sparse_addition_scalar(vec_int, 0))
  )
  expect_true(
    is_sparse_double(sparse_addition_scalar(vec_double, 0))
  )
  expect_true(
    is_sparse_integer(sparse_addition_scalar(vec_int, 1))
  )
  expect_true(
    is_sparse_double(sparse_addition_scalar(vec_double, 1))
  )
  expect_true(
    is_sparse_integer(sparse_addition_scalar(vec_int, 5))
  )
  expect_true(
    is_sparse_double(sparse_addition_scalar(vec_double, 5))
  )
})


test_that("scalar subtraction works", {
  vec_int <- sparse_integer(c(4, 78), c(1, 10), 10)
  vec_double <- sparse_double(c(4, 78), c(1, 10), 10)

  exp_int <- as.integer(c(4, 0, 0, 0, 0, 0, 0, 0, 0, 78))
  exp_double <- c(4, 0, 0, 0, 0, 0, 0, 0, 0, 78)


  expect_identical(
    sparse_subtraction_scalar(vec_int, 0),
    exp_int - 0L
  )
  expect_identical(
    sparse_subtraction_scalar(vec_double, 0),
    exp_double - 0
  )
  expect_identical(
    sparse_subtraction_scalar(vec_int, 1),
    exp_int - 1L
  )
  expect_identical(
    sparse_subtraction_scalar(vec_double, 1),
    exp_double - 1
  )
  expect_identical(
    sparse_subtraction_scalar(vec_int, 5),
    exp_int - 5L
  )
  expect_identical(
    sparse_subtraction_scalar(vec_double, 5),
    exp_double - 5
  )

  expect_identical(
    sparse_subtraction_scalar(vec_double, 5.5623),
    exp_double - 5.5623
  )

  expect_true(
    is_sparse_integer(sparse_subtraction_scalar(vec_int, 0))
  )
  expect_true(
    is_sparse_double(sparse_subtraction_scalar(vec_double, 0))
  )
  expect_true(
    is_sparse_integer(sparse_subtraction_scalar(vec_int, 1))
  )
  expect_true(
    is_sparse_double(sparse_subtraction_scalar(vec_double, 1))
  )
  expect_true(
    is_sparse_integer(sparse_subtraction_scalar(vec_int, 5))
  )
  expect_true(
    is_sparse_double(sparse_subtraction_scalar(vec_double, 5))
  )
})

test_that("vector multiplication works - integers", {
  sparse_int_1 <- sparse_integer(c(4, 78), c(1, 10), 10)
  sparse_int_2 <- sparse_integer(c(4, 78), c(1, 9), 10)
  sparse_int_3 <- sparse_integer(c(4, 3, 4, 5), c(2, 3, 5, 9), 10)

  dense_int_1 <- sparse_int_1[]
  dense_int_2 <- sparse_int_2[]
  dense_int_3 <- sparse_int_3[]

  # integer, sparse x sparse
  expect_identical(
    sparse_multiplication(sparse_int_1, sparse_int_1),
    sparse_int_1 * sparse_int_1
  )
  expect_identical(
    sparse_multiplication(sparse_int_1, sparse_int_2),
    sparse_int_1 * sparse_int_2
  )
  expect_identical(
    sparse_multiplication(sparse_int_1, sparse_int_3),
    sparse_int_1 * sparse_int_3
  )
  # integer, dense x dense
  expect_identical(
    sparse_multiplication(dense_int_1, dense_int_1),
    dense_int_1 * dense_int_1
  )
  expect_identical(
    sparse_multiplication(dense_int_1, dense_int_2),
    dense_int_1 * dense_int_2
  )
  expect_identical(
    sparse_multiplication(dense_int_1, dense_int_3),
    dense_int_1 * dense_int_3
  )
  # integer, sparse x dense
  expect_identical(
    sparse_multiplication(sparse_int_1, dense_int_1),
    sparse_int_1 * dense_int_1
  )
  expect_identical(
    sparse_multiplication(sparse_int_1, dense_int_2),
    sparse_int_1 * dense_int_2
  )
  expect_identical(
    sparse_multiplication(sparse_int_1, dense_int_3),
    sparse_int_1 * dense_int_3
  )
  # integer, dense x sparse
  expect_identical(
    sparse_multiplication(dense_int_1, sparse_int_1),
    dense_int_1 * sparse_int_1
  )
  expect_identical(
    sparse_multiplication(dense_int_1, sparse_int_2),
    dense_int_1 * sparse_int_2
  )
  expect_identical(
    sparse_multiplication(dense_int_1, sparse_int_3),
    dense_int_1 * sparse_int_3
  )
})

test_that("vector multiplication works - doubles", {
  sparse_dbl_1 <- sparse_double(c(4.3, 7.8), c(1, 10), 10)
  sparse_dbl_2 <- sparse_double(c(4.3, 7.8), c(1, 9), 10)
  sparse_dbl_3 <- sparse_double(c(4.2, 3.1, 4.4, 5.5), c(2, 3, 5, 9), 10)

  dense_dbl_1 <- sparse_dbl_1[]
  dense_dbl_2 <- sparse_dbl_2[]
  dense_dbl_3 <- sparse_dbl_3[]

  # double, sparse x sparse
  expect_identical(
    sparse_multiplication(sparse_dbl_1, sparse_dbl_1),
    sparse_dbl_1 * sparse_dbl_1
  )
  expect_identical(
    sparse_multiplication(sparse_dbl_1, sparse_dbl_2),
    sparse_dbl_1 * sparse_dbl_2
  )
  expect_identical(
    sparse_multiplication(sparse_dbl_1, sparse_dbl_3),
    sparse_dbl_1 * sparse_dbl_3
  )
  # double, dense x dense
  expect_identical(
    sparse_multiplication(dense_dbl_1, dense_dbl_1),
    dense_dbl_1 * dense_dbl_1
  )
  expect_identical(
    sparse_multiplication(dense_dbl_1, dense_dbl_2),
    dense_dbl_1 * dense_dbl_2
  )
  expect_identical(
    sparse_multiplication(dense_dbl_1, dense_dbl_3),
    dense_dbl_1 * dense_dbl_3
  )
  # double, sparse x dense
  expect_identical(
    sparse_multiplication(sparse_dbl_1, dense_dbl_1),
    sparse_dbl_1 * dense_dbl_1
  )
  expect_identical(
    sparse_multiplication(sparse_dbl_1, dense_dbl_2),
    sparse_dbl_1 * dense_dbl_2
  )
  expect_identical(
    sparse_multiplication(sparse_dbl_1, dense_dbl_3),
    sparse_dbl_1 * dense_dbl_3
  )
  # double, dense x sparse
  expect_identical(
    sparse_multiplication(dense_dbl_1, sparse_dbl_1),
    dense_dbl_1 * sparse_dbl_1
  )
  expect_identical(
    sparse_multiplication(dense_dbl_1, sparse_dbl_2),
    dense_dbl_1 * sparse_dbl_2
  )
  expect_identical(
    sparse_multiplication(dense_dbl_1, sparse_dbl_3),
    dense_dbl_1 * sparse_dbl_3
  )
})
