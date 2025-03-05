test_that("sparse_lag() works with doubles", {
  vec <- sparse_double(c(pi, 4, 5 / 2), c(1, 5, 7), 10)

  expect_true(is_sparse_double(sparse_lag(vec, 1)))
  expect_true(is_sparse_double(sparse_lag(vec, 4)))
  expect_true(is_sparse_double(sparse_lag(vec, 100)))

  expect_identical(
    sparse_lag(vec, 1),
    c(rep(NA, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5),
    c(rep(NA, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100),
    rep(NA_real_, 10)
  )

  expect_true(is_sparse_double(sparse_lag(vec, 1, 0)))
  expect_true(is_sparse_double(sparse_lag(vec, 4, 0)))
  expect_true(is_sparse_double(sparse_lag(vec, 100, 0)))

  expect_identical(
    sparse_lag(vec, 1, 0),
    c(rep(0, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5, 0),
    c(rep(0, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100, 0),
    rep(0, 10)
  )
})

test_that("sparse_lag() works with doubles - non-zero default", {
  vec <- sparse_double(c(pi, 3, 5 / 2), c(1, 5, 7), 10, default = 4)

  expect_true(is_sparse_double(sparse_lag(vec, 1)))
  expect_true(is_sparse_double(sparse_lag(vec, 4)))
  expect_true(is_sparse_double(sparse_lag(vec, 100)))

  expect_identical(
    sparse_lag(vec, 1),
    c(rep(NA, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5),
    c(rep(NA, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100),
    rep(NA_real_, 10)
  )

  expect_true(is_sparse_double(sparse_lag(vec, 1, 0)))
  expect_true(is_sparse_double(sparse_lag(vec, 4, 0)))
  expect_true(is_sparse_double(sparse_lag(vec, 100, 0)))

  expect_identical(
    sparse_lag(vec, 1, 0),
    c(rep(0, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5, 0),
    c(rep(0, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100, 0),
    rep(0, 10)
  )
})

test_that("sparse_lag() works with integers", {
  vec <- sparse_integer(c(3, 4, 5), c(1, 5, 7), 10)

  expect_true(is_sparse_integer(sparse_lag(vec, 1)))
  expect_true(is_sparse_integer(sparse_lag(vec, 4)))
  expect_true(is_sparse_integer(sparse_lag(vec, 100)))

  expect_identical(
    sparse_lag(vec, 1),
    c(rep(NA, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5),
    c(rep(NA, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100),
    rep(NA_integer_, 10)
  )

  expect_true(is_sparse_integer(sparse_lag(vec, 1, 0L)))
  expect_true(is_sparse_integer(sparse_lag(vec, 4, 0L)))
  expect_true(is_sparse_integer(sparse_lag(vec, 100, 0L)))

  expect_identical(
    sparse_lag(vec, 1, 0L),
    c(rep(0L, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5, 0L),
    c(rep(0L, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100, 0L),
    rep(0L, 10)
  )
})

test_that("sparse_lag() works with integers - non-zero default", {
  vec <- sparse_integer(c(3, 7, 5), c(1, 5, 7), 10, 4)

  expect_true(is_sparse_integer(sparse_lag(vec, 1)))
  expect_true(is_sparse_integer(sparse_lag(vec, 4)))
  expect_true(is_sparse_integer(sparse_lag(vec, 100)))

  expect_identical(
    sparse_lag(vec, 1),
    c(rep(NA, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5),
    c(rep(NA, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100),
    rep(NA_integer_, 10)
  )

  expect_true(is_sparse_integer(sparse_lag(vec, 1, 0L)))
  expect_true(is_sparse_integer(sparse_lag(vec, 4, 0L)))
  expect_true(is_sparse_integer(sparse_lag(vec, 100, 0L)))

  expect_identical(
    sparse_lag(vec, 1, 0L),
    c(rep(0L, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5, 0L),
    c(rep(0L, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100, 0L),
    rep(0L, 10)
  )
})

test_that("sparse_lag() works with character", {
  vec <- sparse_character(c("A", "B", "C"), c(1, 5, 7), 10)

  expect_true(is_sparse_character(sparse_lag(vec, 1)))
  expect_true(is_sparse_character(sparse_lag(vec, 4)))
  expect_true(is_sparse_character(sparse_lag(vec, 100)))

  expect_identical(
    sparse_lag(vec, 1),
    c(rep(NA, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5),
    c(rep(NA, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100),
    rep(NA_character_, 10)
  )

  expect_true(is_sparse_character(sparse_lag(vec, 1, "before")))
  expect_true(is_sparse_character(sparse_lag(vec, 4, "before")))
  expect_true(is_sparse_character(sparse_lag(vec, 100, "before")))

  expect_identical(
    sparse_lag(vec, 1, "before"),
    c(rep("before", 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5, "before"),
    c(rep("before", 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100, "before"),
    rep("before", 10)
  )
})

test_that("sparse_lag() works with character - non-zero default", {
  vec <- sparse_character(c("A", "B", "C"), c(1, 5, 7), 10, "before")

  expect_true(is_sparse_character(sparse_lag(vec, 1)))
  expect_true(is_sparse_character(sparse_lag(vec, 4)))
  expect_true(is_sparse_character(sparse_lag(vec, 100)))

  expect_identical(
    sparse_lag(vec, 1),
    c(rep(NA, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5),
    c(rep(NA, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100),
    rep(NA_character_, 10)
  )

  expect_true(is_sparse_character(sparse_lag(vec, 1, "before")))
  expect_true(is_sparse_character(sparse_lag(vec, 4, "before")))
  expect_true(is_sparse_character(sparse_lag(vec, 100, "before")))

  expect_identical(
    sparse_lag(vec, 1, "before"),
    c(rep("before", 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5, "before"),
    c(rep("before", 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100, "before"),
    rep("before", 10)
  )
})

test_that("sparse_lag() works with logical", {
  vec <- sparse_logical(c(TRUE, TRUE, TRUE), c(1, 5, 7), 10)

  expect_true(is_sparse_logical(sparse_lag(vec, 1)))
  expect_true(is_sparse_logical(sparse_lag(vec, 4)))
  expect_true(is_sparse_logical(sparse_lag(vec, 100)))

  expect_identical(
    sparse_lag(vec, 1),
    c(rep(NA, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5),
    c(rep(NA, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100),
    rep(NA, 10)
  )

  expect_true(is_sparse_logical(sparse_lag(vec, 1, TRUE)))
  expect_true(is_sparse_logical(sparse_lag(vec, 4, TRUE)))
  expect_true(is_sparse_logical(sparse_lag(vec, 100, TRUE)))

  expect_identical(
    sparse_lag(vec, 1, TRUE),
    c(rep(TRUE, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5, TRUE),
    c(rep(TRUE, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100, TRUE),
    rep(TRUE, 10)
  )
})

test_that("sparse_lag() works with logical - non-zero default", {
  vec <- sparse_logical(c(FALSE, FALSE, FALSE), c(1, 5, 7), 10, TRUE)

  expect_true(is_sparse_logical(sparse_lag(vec, 1)))
  expect_true(is_sparse_logical(sparse_lag(vec, 4)))
  expect_true(is_sparse_logical(sparse_lag(vec, 100)))

  expect_identical(
    sparse_lag(vec, 1),
    c(rep(NA, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5),
    c(rep(NA, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100),
    rep(NA, 10)
  )

  expect_true(is_sparse_logical(sparse_lag(vec, 1, TRUE)))
  expect_true(is_sparse_logical(sparse_lag(vec, 4, TRUE)))
  expect_true(is_sparse_logical(sparse_lag(vec, 100, TRUE)))

  expect_identical(
    sparse_lag(vec, 1, TRUE),
    c(rep(TRUE, 1), head(vec, -1))
  )
  expect_identical(
    sparse_lag(vec, 5, TRUE),
    c(rep(TRUE, 5), head(vec, -5))
  )
  expect_identical(
    sparse_lag(vec, 100, TRUE),
    rep(TRUE, 10)
  )
})
