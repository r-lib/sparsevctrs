# one_hot = TRUE --------------------------------------------------------------

test_that("sparse_dummy(one_hot = TRUE) works with single level", {
  x <- factor(c("a", "a", "a"))
  exp <- list(
    a = c(1L, 1L, 1L)
  )

  res <- sparse_dummy(x, one_hot = TRUE)
  expect_identical(
    res,
    exp
  )

  expect_true(is.integer(res$a))
  expect_false(is_sparse_vector(res$a))
})

test_that("sparse_dummy(one_hot = FALSE) works zero length input", {
  x <- factor(character())
  exp <- structure(list(), names = character(0))

  res <- sparse_dummy(x, one_hot = FALSE)
  expect_identical(
    res,
    exp
  )
})

## anyNA = FALSE ---------------------------------------------------------------

test_that("sparse_dummy(one_hot = TRUE) works with no NAs", {
  x <- factor(c("a", "b", "c", "d", "a"))
  exp <- list(
    a = sparse_integer(c(1, 1), c(1, 5), 5),
    b = sparse_integer(1, 2, 5),
    c = sparse_integer(1, 3, 5),
    d = sparse_integer(1, 4, 5)
  )

  res <- sparse_dummy(x, one_hot = TRUE)
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})

test_that("sparse_dummy(one_hot = TRUE) works with no NAs and unseen levels", {
  x <- factor(c("a", "b", "c", "d", "a"), levels = letters[1:6])
  exp <- list(
    a = sparse_integer(c(1, 1), c(1, 5), 5),
    b = sparse_integer(1, 2, 5),
    c = sparse_integer(1, 3, 5),
    d = sparse_integer(1, 4, 5),
    e = sparse_integer(integer(), integer(), 5),
    f = sparse_integer(integer(), integer(), 5)
  )

  res <- sparse_dummy(x, one_hot = TRUE)
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})

## anyNA = TRUE ----------------------------------------------------------------

test_that("sparse_dummy(one_hot = TRUE) works with NA", {
  x <- factor(c("a", NA, "b", "c", "a", NA))
  exp <- list(
    a = sparse_integer(c(1, NA, 1, NA), c(1, 2, 5, 6), 6),
    b = sparse_integer(c(NA, 1, NA), c(2, 3, 6), 6),
    c = sparse_integer(c(NA, 1, NA), c(2, 4, 6), 6)
  )

  res <- sparse_dummy(x, one_hot = TRUE)
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})

test_that("sparse_dummy(one_hot = TRUE) works with NA and unseen levels", {
  x <- factor(c("a", NA, "b", "c", "a", NA), levels = letters[1:5])
  exp <- list(
    a = sparse_integer(c(1, NA, 1, NA), c(1, 2, 5, 6), 6),
    b = sparse_integer(c(NA, 1, NA), c(2, 3, 6), 6),
    c = sparse_integer(c(NA, 1, NA), c(2, 4, 6), 6),
    d = sparse_integer(c(NA, NA), c(2, 6), 6),
    e = sparse_integer(c(NA, NA), c(2, 6), 6)
  )

  res <- sparse_dummy(x, one_hot = TRUE)
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})

# one_hot = FALSE ---------------------------------------------------------------

test_that("sparse_dummy(one_hot = FALSE) works with single level", {
  x <- factor(c("a", "a", "a"))
  exp <- structure(list(), names = character(0))

  res <- sparse_dummy(x, one_hot = FALSE)

  expect_identical(
    res,
    exp
  )
})

test_that("sparse_dummy(one_hot = FALSE) works with two levels", {
  x <- factor(c("a", "b", "a"))
  exp <- list(
    b = c(0L, 1L, 0L)
  )

  res <- sparse_dummy(x, one_hot = FALSE)
  expect_identical(
    res,
    exp
  )

  expect_true(is.integer(res$b))
  expect_true(is_sparse_vector(res$b))
})

test_that("sparse_dummy(one_hot = TRUE) works zero length input", {
  x <- factor(character())
  exp <- structure(list(), names = character(0))

  res <- sparse_dummy(x, one_hot = FALSE)
  expect_identical(
    res,
    exp
  )
})

## anyNA = FALSE ---------------------------------------------------------------

test_that("sparse_dummy(one_hot = FALSE) works with no NAs", {
  x <- factor(c("a", "b", "c", "d", "a"))
  exp <- list(
    b = sparse_integer(1, 2, 5),
    c = sparse_integer(1, 3, 5),
    d = sparse_integer(1, 4, 5)
  )

  res <- sparse_dummy(x, one_hot = FALSE)
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})

test_that("sparse_dummy(one_hot = FALSE) works with no NAs and unseen levels", {
  x <- factor(c("a", "b", "c", "d", "a"), levels = letters[1:6])
  exp <- list(
    b = sparse_integer(1, 2, 5),
    c = sparse_integer(1, 3, 5),
    d = sparse_integer(1, 4, 5),
    e = sparse_integer(integer(), integer(), 5),
    f = sparse_integer(integer(), integer(), 5)
  )

  res <- sparse_dummy(x, one_hot = FALSE)
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})

## anyNA = TRUE ----------------------------------------------------------------

test_that("sparse_dummy(one_hot = FALSE) works with NA", {
  x <- factor(c("a", NA, "b", "c", "a", NA))
  exp <- list(
    b = sparse_integer(c(NA, 1, NA), c(2, 3, 6), 6),
    c = sparse_integer(c(NA, 1, NA), c(2, 4, 6), 6)
  )

  res <- sparse_dummy(x, one_hot = FALSE)
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})

test_that("sparse_dummy(one_hot = FALSE) works with NA and unseen levels", {
  x <- factor(c("a", NA, "b", "c", "a", NA), levels = letters[1:5])
  exp <- list(
    b = sparse_integer(c(NA, 1, NA), c(2, 3, 6), 6),
    c = sparse_integer(c(NA, 1, NA), c(2, 4, 6), 6),
    d = sparse_integer(c(NA, NA), c(2, 6), 6),
    e = sparse_integer(c(NA, NA), c(2, 6), 6)
  )

  res <- sparse_dummy(x, one_hot = FALSE)
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})

# Other ------------------------------------------------------------------------

test_that("sparse_dummy() errors with wrong input", {
  expect_snapshot(
    error = TRUE,
    sparse_dummy(letters)
  )
  expect_snapshot(
    error = TRUE,
    sparse_dummy(mtcars)
  )
  expect_snapshot(
    error = TRUE,
    sparse_dummy(1:5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_dummy(NULL)
  )
})
