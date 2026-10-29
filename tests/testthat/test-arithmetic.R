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

  expect_identical(
    sparse_multiplication_scalar(vec_int, NA),
    rep(NA_integer_, length(vec_int))
  )
  expect_identical(
    sparse_multiplication_scalar(vec_double, NA),
    rep(NA_real_, length(vec_double))
  )
})

test_that("scalar division works", {
  vec_int <- sparse_integer(c(4, 78), c(1, 10), 10)
  vec_double <- sparse_double(c(4, 78), c(1, 10), 10)

  exp_double <- c(4, 0, 0, 0, 0, 0, 0, 0, 0, 78)

  expect_identical(
    sparse_division_scalar(vec_int, 0),
    exp_double / 0
  )
  expect_identical(
    sparse_division_scalar(vec_double, 0),
    exp_double / 0
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

  expect_identical(
    sparse_division_scalar(vec_int, NA),
    rep(NA_integer_, length(vec_int))
  )
  expect_identical(
    sparse_division_scalar(vec_double, NA),
    rep(NA_real_, length(vec_double))
  )
})

test_that("scalar division by 0 matches dense division", {
  vec_int <- sparse_integer(c(-4L, NA, 5L), c(1, 5, 10), 10)
  vec_dbl <- sparse_double(c(-4, NA, 5), c(1, 5, 10), 10)
  vec_int_neg <- sparse_integer(c(-4L, 5L), c(1, 10), 10, default = -3L)
  vec_dbl_pos <- sparse_double(c(-4, 5), c(1, 10), 10, default = 3)
  empty_dbl <- sparse_double(numeric(), integer(), 5, default = 3)
  len_0 <- sparse_double(numeric(), integer(), 0)

  expect_identical(sparse_division_scalar(vec_int, 0), vec_int[] / 0)
  expect_identical(sparse_division_scalar(vec_dbl, 0), vec_dbl[] / 0)
  expect_identical(sparse_division_scalar(vec_int_neg, 0), vec_int_neg[] / 0)
  expect_identical(sparse_division_scalar(vec_dbl_pos, 0), vec_dbl_pos[] / 0)
  expect_identical(sparse_division_scalar(empty_dbl, 0), empty_dbl[] / 0)
  expect_identical(sparse_division_scalar(len_0, 0), len_0[] / 0)

  expect_false(is_sparse_vector(sparse_division_scalar(vec_dbl, 0)))
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

  expect_identical(
    sparse_addition_scalar(vec_int, NA),
    rep(NA_integer_, length(vec_int))
  )
  expect_identical(
    sparse_addition_scalar(vec_double, NA),
    rep(NA_real_, length(vec_double))
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

  expect_identical(
    sparse_subtraction_scalar(vec_int, NA),
    rep(NA_integer_, length(vec_int))
  )
  expect_identical(
    sparse_subtraction_scalar(vec_double, NA),
    rep(NA_real_, length(vec_double))
  )
})

test_that("scalar multiplication by 0 keeps stored NA values", {
  vec_int <- sparse_integer(c(4L, NA, 5L), c(1, 5, 10), 10)
  vec_dbl <- sparse_double(c(4, NA, 5), c(1, 5, 10), 10)
  vec_int_3 <- sparse_integer(c(NA, 5L), c(2, 5), 10, default = 3L)
  vec_dbl_3 <- sparse_double(c(NA, 5), c(2, 5), 10, default = 3)

  expect_identical(sparse_multiplication_scalar(vec_int, 0), vec_int[] * 0L)
  expect_identical(sparse_multiplication_scalar(vec_dbl, 0), vec_dbl[] * 0)
  expect_identical(sparse_multiplication_scalar(vec_int_3, 0), vec_int_3[] * 0L)
  expect_identical(sparse_multiplication_scalar(vec_dbl_3, 0), vec_dbl_3[] * 0)

  expect_true(is_sparse_integer(sparse_multiplication_scalar(vec_int, 0)))
  expect_true(is_sparse_double(sparse_multiplication_scalar(vec_dbl, 0)))
})

test_that("scalar arithmetic works with non-zero defaults", {
  vec_int <- sparse_integer(c(4L, 78L), c(1, 10), 10, default = 3L)
  vec_dbl <- sparse_double(c(4, 78), c(1, 10), 10, default = 3)
  empty_dbl <- sparse_double(numeric(), integer(), 5, default = 3)

  expect_identical(sparse_multiplication_scalar(vec_int, 2), vec_int[] * 2L)
  expect_identical(sparse_multiplication_scalar(vec_int, 0), vec_int[] * 0L)
  expect_identical(sparse_multiplication_scalar(vec_dbl, 2), vec_dbl[] * 2)
  expect_identical(sparse_multiplication_scalar(vec_dbl, 0), vec_dbl[] * 0)
  expect_identical(sparse_multiplication_scalar(empty_dbl, 2), empty_dbl[] * 2)

  expect_identical(sparse_division_scalar(vec_int, 2), vec_int[] / 2)
  expect_identical(sparse_division_scalar(vec_dbl, 2), vec_dbl[] / 2)
  expect_identical(sparse_division_scalar(empty_dbl, 2), empty_dbl[] / 2)

  expect_identical(sparse_addition_scalar(empty_dbl, 2), empty_dbl[] + 2)
  expect_identical(sparse_subtraction_scalar(empty_dbl, 2), empty_dbl[] - 2)
})

test_that("scalar arithmetic errors for non-numeric sparse vectors", {
  vec_lgl <- sparse_logical(TRUE, 1, 3)
  vec_chr <- sparse_character("a", 1, 3)

  expect_snapshot(error = TRUE, sparse_multiplication_scalar(vec_lgl, 2))
  expect_snapshot(error = TRUE, sparse_multiplication_scalar(vec_chr, 2))
  expect_snapshot(error = TRUE, sparse_division_scalar(vec_lgl, 2))
  expect_snapshot(error = TRUE, sparse_division_scalar(vec_chr, 2))
  expect_snapshot(error = TRUE, sparse_addition_scalar(vec_lgl, 2))
  expect_snapshot(error = TRUE, sparse_addition_scalar(vec_chr, 2))
  expect_snapshot(error = TRUE, sparse_subtraction_scalar(vec_lgl, 2))
  expect_snapshot(error = TRUE, sparse_subtraction_scalar(vec_chr, 2))
})

test_that("scalar arithmetic errors for non-numeric val", {
  vec_dbl <- sparse_double(c(4, 78), c(1, 10), 10)

  expect_snapshot(error = TRUE, sparse_addition_scalar(vec_dbl, "a"))
})

test_that("scalar arithmetic keeps stored NA values", {
  vec_int <- sparse_integer(c(4L, NA), c(1, 5), 10)
  vec_dbl <- sparse_double(c(4, NA), c(1, 5), 10)

  expect_identical(sparse_multiplication_scalar(vec_int, 2), vec_int[] * 2L)
  expect_identical(sparse_division_scalar(vec_int, 2), vec_int[] / 2)
  expect_identical(sparse_addition_scalar(vec_int, 2), vec_int[] + 2L)
  expect_identical(sparse_subtraction_scalar(vec_int, 2), vec_int[] - 2L)

  expect_identical(sparse_multiplication_scalar(vec_dbl, 2), vec_dbl[] * 2)
  expect_identical(sparse_division_scalar(vec_dbl, 2), vec_dbl[] / 2)
  expect_identical(sparse_addition_scalar(vec_dbl, 2), vec_dbl[] + 2)
  expect_identical(sparse_subtraction_scalar(vec_dbl, 2), vec_dbl[] - 2)
})

test_that("scalar arithmetic on sparse integers needs whole number results", {
  vec_int <- sparse_integer(c(4L, 78L), c(1, 10), 10)

  res <- sparse_multiplication_scalar(vec_int, 0.5)
  expect_identical(res, as.integer(vec_int[] * 0.5))
  expect_true(is_sparse_integer(res))

  expect_snapshot(error = TRUE, sparse_multiplication_scalar(vec_int, 0.3))
  expect_snapshot(error = TRUE, sparse_addition_scalar(vec_int, 0.5))
})

test_that("scalar arithmetic on sparse integers handles overflow", {
  vec_int <- sparse_integer(c(.Machine$integer.max, 2L), c(1, 3), 5)

  expect_warning(
    res <- sparse_addition_scalar(vec_int, 1L),
    "integer overflow"
  )
  expect_identical(res, c(NA, 1L, 3L, 1L, 1L))
  expect_true(is_sparse_integer(res))

  expect_warning(
    res <- sparse_multiplication_scalar(vec_int, 2L),
    "integer overflow"
  )
  expect_identical(res, c(NA, 0L, 4L, 0L, 0L))

  vec_default <- sparse_integer(1L, 1, 5, default = .Machine$integer.max)
  expect_snapshot(error = TRUE, sparse_addition_scalar(vec_default, 1L))
})

test_that("scalar arithmetic errors for results that can't be stored", {
  vec_dbl <- sparse_double(c(4, 78), c(1, 10), 10)

  expect_snapshot(error = TRUE, sparse_multiplication_scalar(vec_dbl, Inf))
  expect_snapshot(error = TRUE, sparse_addition_scalar(vec_dbl, Inf))
})

test_that("vector multiplication works - integers", {
  sparse_int_1 <- sparse_integer(c(4, 78), c(1, 10), 10)
  sparse_int_2 <- sparse_integer(c(4, 78), c(1, 9), 10)
  sparse_int_3 <- sparse_integer(c(4, 3, 4, 5), c(2, 3, 5, 9), 10)
  sparse_int_NA <- sparse_integer(c(4, NA, 4, 5), c(2, 3, 5, 9), 10)
  sparse_int_NA_2 <- sparse_integer(c(4, NA, 4, 5), c(2, 4, 5, 9), 10)

  dense_int_1 <- sparse_int_1[]
  dense_int_2 <- sparse_int_2[]
  dense_int_3 <- sparse_int_3[]
  dense_int_NA <- sparse_int_NA[]
  dense_int_NA_2 <- sparse_int_NA_2[]

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
  expect_identical(
    sparse_multiplication(sparse_int_1, sparse_int_NA),
    sparse_int_1 * sparse_int_NA
  )
  expect_identical(
    sparse_multiplication(sparse_int_3, sparse_int_NA),
    sparse_int_3 * sparse_int_NA
  )
  expect_identical(
    sparse_multiplication(sparse_int_NA, sparse_int_NA_2),
    sparse_int_NA * sparse_int_NA_2
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
  expect_identical(
    sparse_multiplication(dense_int_1, dense_int_NA),
    dense_int_1 * dense_int_NA
  )
  expect_identical(
    sparse_multiplication(dense_int_3, dense_int_NA),
    dense_int_3 * dense_int_NA
  )
  expect_identical(
    sparse_multiplication(dense_int_NA, dense_int_NA_2),
    dense_int_NA * dense_int_NA_2
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
  expect_identical(
    sparse_multiplication(sparse_int_1, dense_int_NA),
    sparse_int_1 * dense_int_NA
  )
  expect_identical(
    sparse_multiplication(sparse_int_3, dense_int_NA),
    sparse_int_3 * dense_int_NA
  )
  expect_identical(
    sparse_multiplication(sparse_int_NA, dense_int_NA_2),
    sparse_int_NA * dense_int_NA_2
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
  expect_identical(
    sparse_multiplication(dense_int_1, sparse_int_NA),
    dense_int_1 * sparse_int_NA
  )
  expect_identical(
    sparse_multiplication(dense_int_3, sparse_int_NA),
    dense_int_3 * sparse_int_NA
  )
  expect_identical(
    sparse_multiplication(dense_int_NA, sparse_int_NA_2),
    dense_int_NA * sparse_int_NA_2
  )
})

test_that("vector multiplication works - doubles", {
  sparse_dbl_1 <- sparse_double(c(4.3, 7.8), c(1, 10), 10)
  sparse_dbl_2 <- sparse_double(c(4.3, 7.8), c(1, 9), 10)
  sparse_dbl_3 <- sparse_double(c(4.2, 3.1, 4.4, 5.5), c(2, 3, 5, 9), 10)
  sparse_dbl_NA <- sparse_double(c(4.2, NA, 4.4, 5.5), c(2, 3, 5, 9), 10)
  sparse_dbl_NA_2 <- sparse_double(c(4.2, NA, 4.4, 5.5), c(2, 4, 5, 9), 10)

  dense_dbl_1 <- sparse_dbl_1[]
  dense_dbl_2 <- sparse_dbl_2[]
  dense_dbl_3 <- sparse_dbl_3[]
  dense_dbl_NA <- sparse_dbl_NA[]
  dense_dbl_NA_2 <- sparse_dbl_NA_2[]

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
  expect_identical(
    sparse_multiplication(sparse_dbl_1, sparse_dbl_NA),
    sparse_dbl_1 * sparse_dbl_NA
  )
  expect_identical(
    sparse_multiplication(sparse_dbl_3, sparse_dbl_NA),
    sparse_dbl_3 * sparse_dbl_NA
  )
  expect_identical(
    sparse_multiplication(sparse_dbl_NA, sparse_dbl_NA_2),
    sparse_dbl_NA * sparse_dbl_NA_2
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
  expect_identical(
    sparse_multiplication(dense_dbl_1, dense_dbl_NA),
    dense_dbl_1 * dense_dbl_NA
  )
  expect_identical(
    sparse_multiplication(dense_dbl_3, dense_dbl_NA),
    dense_dbl_3 * dense_dbl_NA
  )
  expect_identical(
    sparse_multiplication(dense_dbl_NA, dense_dbl_NA_2),
    dense_dbl_NA * dense_dbl_NA_2
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
  expect_identical(
    sparse_multiplication(sparse_dbl_1, dense_dbl_NA),
    sparse_dbl_1 * dense_dbl_NA
  )
  expect_identical(
    sparse_multiplication(sparse_dbl_3, dense_dbl_NA),
    sparse_dbl_3 * dense_dbl_NA
  )
  expect_identical(
    sparse_multiplication(sparse_dbl_NA, dense_dbl_NA_2),
    sparse_dbl_NA * dense_dbl_NA_2
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
  expect_identical(
    sparse_multiplication(dense_dbl_1, sparse_dbl_NA),
    dense_dbl_1 * sparse_dbl_NA
  )
  expect_identical(
    sparse_multiplication(dense_dbl_3, sparse_dbl_NA),
    dense_dbl_3 * sparse_dbl_NA
  )
  expect_identical(
    sparse_multiplication(dense_dbl_NA, sparse_dbl_NA_2),
    dense_dbl_NA * sparse_dbl_NA_2
  )
})

test_that("vector multiplication works - stored NA with fractional dense value", {
  sparse_dbl <- sparse_double(c(NA, 2), c(1, 3), 4)
  dense_dbl <- c(0.5, 1, 3, 4)

  res <- sparse_multiplication(sparse_dbl, dense_dbl)
  expect_identical(sparse_positions(res), c(1L, 3L))
  expect_identical(res[], sparse_dbl[] * dense_dbl)

  res <- sparse_multiplication(dense_dbl, sparse_dbl)
  expect_identical(sparse_positions(res), c(1L, 3L))
  expect_identical(res[], sparse_dbl[] * dense_dbl)
})

test_that("vector multiplication works - output positions are sorted", {
  x <- sparse_integer(c(1L, NA, 3L), c(2, 5, 7), 8)
  y <- sparse_integer(c(NA, 2L, 4L), c(1, 2, 7), 8)
  res <- sparse_multiplication(x, y)
  expect_identical(sparse_positions(res), c(1L, 2L, 5L, 7L))
  expect_identical(res[], x[] * y[])

  x <- sparse_integer(c(3L, NA, 1L), c(2, 5, 7), 8)
  y <- sparse_integer(c(NA, 2L, 4L), c(1, 2, 7), 8)
  res <- sparse_multiplication(x, y)
  expect_identical(res[], x[] * y[])

  set.seed(1234)
  n <- 500
  pos_x <- sort(sample.int(n, 200))
  pos_y <- sort(sample.int(n, 200))

  val_x <- sample(c(NA, 1:5), 200, replace = TRUE)
  val_y <- sample(c(NA, 1:5), 200, replace = TRUE)
  x <- sparse_integer(val_x, pos_x, n)
  y <- sparse_integer(val_y, pos_y, n)
  res <- sparse_multiplication(x, y)
  expect_false(is.unsorted(sparse_positions(res)))
  expect_identical(res[], x[] * y[])
  res <- sparse_multiplication(x, y[])
  expect_false(is.unsorted(sparse_positions(res)))
  expect_identical(res[], x[] * y[])

  x <- sparse_double(as.double(val_x), pos_x, n)
  y <- sparse_double(as.double(val_y), pos_y, n)
  res <- sparse_multiplication(x, y)
  expect_false(is.unsorted(sparse_positions(res)))
  expect_identical(res[], x[] * y[])
  res <- sparse_multiplication(x, y[])
  expect_false(is.unsorted(sparse_positions(res)))
  expect_identical(res[], x[] * y[])
})

test_that("vector multiplication works - types", {
  sparse_int <- sparse_integer(c(4, 78), c(1, 10), 10)
  dense_int <- sparse_int[]
  sparse_dbl <- sparse_double(c(4.3, 7.8), c(1, 10), 10)
  dense_dbl <- sparse_dbl[]

  res <- sparse_multiplication(sparse_int, sparse_int)
  expect_true(is_sparse_vector(res))
  expect_true(is.integer(res))

  res <- sparse_multiplication(sparse_int, dense_int)
  expect_true(is_sparse_vector(res))
  expect_true(is.integer(res))

  res <- sparse_multiplication(sparse_int, sparse_dbl)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_int, dense_dbl)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(dense_int, sparse_int)
  expect_true(is_sparse_vector(res))
  expect_true(is.integer(res))

  res <- sparse_multiplication(dense_int, dense_int)
  expect_false(is_sparse_vector(res))
  expect_true(is.integer(res))

  res <- sparse_multiplication(dense_int, sparse_dbl)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(dense_int, dense_dbl)
  expect_false(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl, sparse_int)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl, dense_int)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl, sparse_dbl)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl, dense_dbl)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(dense_dbl, sparse_int)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(dense_dbl, dense_int)
  expect_false(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(dense_dbl, sparse_dbl)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(dense_dbl, dense_dbl)
  expect_false(is_sparse_vector(res))
  expect_true(is.double(res))
})

test_that("vector multiplication works - non-zero defaults", {
  sparse_int_0 <- sparse_integer(c(4, 78), c(1, 10), 10, default = 0)
  sparse_int_1 <- sparse_integer(c(4, 78), c(1, 10), 10, default = 1)
  sparse_dbl_0 <- sparse_double(c(4.3, 7.8), c(1, 10), 10, default = 0)
  sparse_dbl_1 <- sparse_double(c(4.3, 7.8), c(1, 10), 10, default = 1)

  res <- sparse_multiplication(sparse_int_0, sparse_int_0)
  expect_identical(res, sparse_int_0 * sparse_int_0)
  expect_true(is_sparse_vector(res))
  expect_true(is.integer(res))

  res <- sparse_multiplication(sparse_int_0, sparse_int_1)
  expect_identical(res, sparse_int_0 * sparse_int_1)
  expect_true(is_sparse_vector(res))
  expect_true(is.integer(res))

  res <- sparse_multiplication(sparse_int_0, sparse_dbl_0)
  expect_identical(res, sparse_int_0 * sparse_dbl_0)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_int_0, sparse_dbl_1)
  expect_identical(res, sparse_int_0 * sparse_dbl_1)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_int_1, sparse_int_0)
  expect_identical(res, sparse_int_1 * sparse_int_0)
  expect_true(is_sparse_vector(res))
  expect_true(is.integer(res))

  res <- sparse_multiplication(sparse_int_1, sparse_int_1)
  expect_identical(res, sparse_int_1 * sparse_int_1)
  expect_false(is_sparse_vector(res))
  expect_true(is.integer(res))

  res <- sparse_multiplication(sparse_int_1, sparse_dbl_0)
  expect_identical(res, sparse_int_1 * sparse_dbl_0)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_int_1, sparse_dbl_1)
  expect_identical(res, sparse_int_1 * sparse_dbl_1)
  expect_false(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl_0, sparse_int_0)
  expect_identical(res, sparse_dbl_0 * sparse_int_0)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl_0, sparse_int_1)
  expect_identical(res, sparse_dbl_0 * sparse_int_1)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl_0, sparse_dbl_0)
  expect_identical(res, sparse_dbl_0 * sparse_dbl_0)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl_0, sparse_dbl_1)
  expect_identical(res, sparse_dbl_0 * sparse_dbl_1)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl_1, sparse_int_0)
  expect_identical(res, sparse_dbl_1 * sparse_int_0)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl_1, sparse_int_1)
  expect_identical(res, sparse_dbl_1 * sparse_int_1)
  expect_false(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl_1, sparse_dbl_0)
  expect_identical(res, sparse_dbl_1 * sparse_dbl_0)
  expect_true(is_sparse_vector(res))
  expect_true(is.double(res))

  res <- sparse_multiplication(sparse_dbl_1, sparse_dbl_1)
  expect_identical(res, sparse_dbl_1 * sparse_dbl_1)
  expect_false(is_sparse_vector(res))
  expect_true(is.double(res))
})

test_that("vector multiplication works - no stored values", {
  empty_int <- sparse_integer(integer(), integer(), 5)
  sparse_int <- sparse_integer(c(2L, NA), c(1, 3), 5)

  expect_identical(
    sparse_multiplication(empty_int, sparse_int)[],
    empty_int[] * sparse_int[]
  )
  expect_identical(
    sparse_multiplication(sparse_int, empty_int)[],
    sparse_int[] * empty_int[]
  )

  empty_dbl <- sparse_double(double(), integer(), 5)
  sparse_dbl <- sparse_double(c(2, NA), c(1, 3), 5)

  expect_identical(
    sparse_multiplication(empty_dbl, sparse_dbl)[],
    empty_dbl[] * sparse_dbl[]
  )
  expect_identical(
    sparse_multiplication(sparse_dbl, empty_dbl)[],
    sparse_dbl[] * empty_dbl[]
  )
})

test_that("vector multiplication errors for non-numeric input", {
  expect_snapshot(error = TRUE, sparse_multiplication("a", 1))
  expect_snapshot(error = TRUE, sparse_multiplication(1, "a"))
})

test_that("vector multiplication works - other altrep input", {
  expect_identical(
    sparse_multiplication(1:2, 1:2),
    sparse_multiplication(c(1L, 2L), c(1L, 2L))
  )
})
