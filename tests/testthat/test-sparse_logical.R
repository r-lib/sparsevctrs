test_that("input checking is done correctly", {
  # value
  expect_snapshot(
    error = TRUE,
    sparse_logical("1", 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(1, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(NULL, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(Inf, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(NaN, 1, 1)
  )

  # position
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, 1.5, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, "1", 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, NULL, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, NA, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, Inf, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, NaN, 1)
  )

  # length
  expect_no_error(
    sparse_logical(logical(0), integer(0), 0)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(numeric(0), integer(0), -10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(numeric(0), integer(0), 10000000000)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(logical(0), integer(0), c(1, 10))
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(logical(0), integer(0), 1.5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(logical(0), integer(0), "1")
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(logical(0), integer(0), NA)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(logical(0), integer(0), Inf)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(logical(0), integer(0), NULL)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(logical(0), integer(0), NaN)
  )

  # Length restriction
  expect_snapshot(
    error = TRUE,
    sparse_logical(c(TRUE, TRUE), 1:6, 10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, 1:6, 10)
  )

  # duplicates in position
  expect_snapshot(
    error = TRUE,
    sparse_logical(c(TRUE, TRUE, TRUE, TRUE), c(1, 1, 5, 6), 10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(rep(TRUE, 100), rep(1, 100), 100)
  )

  # Ordered position
  expect_snapshot(
    error = TRUE,
    sparse_logical(c(TRUE, TRUE), c(3, 1), 5)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, 10, 5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(rep(TRUE, 50), seq(25, 74), 50)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, 0, 5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_logical(rep(TRUE, 101), seq(-50, 50), 100)
  )
})

test_that("length() works with sparse_logical()", {
  expect_identical(
    length(sparse_logical(logical(), integer(), 0)),
    0L
  )

  expect_identical(
    length(sparse_logical(TRUE, 1, 10)),
    10L
  )

  expect_identical(
    length(sparse_logical(TRUE, 1, 100)),
    100L
  )
})

test_that("single subsetting works with sparse_logical()", {
  x_sparse <- sparse_logical(
    value = c(TRUE, NA, TRUE),
    position = c(1, 5, 8),
    10
  )
  x_dense <- c(TRUE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, TRUE, FALSE, FALSE)

  for (i in seq_len(10)) {
    expect_identical(x_sparse[i], x_dense[i])
  }

  expect_identical(x_sparse[0], x_dense[0])

  expect_identical(x_sparse[NA_integer_], x_dense[NA_integer_])

  expect_identical(x_sparse[NULL], x_dense[NULL])

  expect_identical(x_sparse[NaN], x_dense[NaN])

  expect_identical(x_sparse[100], x_dense[100])

  expect_identical(x_sparse[Inf], x_dense[Inf])

  expect_identical(x_sparse["not a number"], x_dense["not a number"])

  expect_identical(x_sparse[1.6], x_dense[1.6])
  expect_identical(x_sparse[2.6], x_dense[2.6])
})

test_that("multiple subsetting works with sparse_logical()", {
  x_sparse <- sparse_logical(
    value = c(TRUE, NA, TRUE),
    position = c(1, 5, 8),
    10
  )
  x_dense <- c(TRUE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, TRUE, FALSE, FALSE)

  expect_identical(x_sparse[1:2], x_dense[1:2])

  expect_identical(x_sparse[3:7], x_dense[3:7])

  expect_identical(x_sparse[c(1, 5, 8, 1)], x_dense[c(1, 5, 8, 1)])

  expect_identical(x_sparse[-1], x_dense[-1])

  expect_identical(x_sparse[-c(5:7)], x_dense[-c(5:7)])

  expect_identical(x_sparse[FALSE], x_dense[FALSE])

  expect_identical(x_sparse[TRUE], x_dense[TRUE])

  expect_identical(x_sparse[NA], x_dense[NA])

  expect_identical(x_sparse[c(1, NA, 4)], x_dense[c(1, NA, 4)])

  expect_identical(x_sparse[c(1, NA, 0, 4, 0)], x_dense[c(1, NA, 0, 4, 0)])

  expect_identical(x_sparse[c(1, 11)], x_dense[c(1, 11)])

  expect_identical(x_sparse[c(1, Inf)], x_dense[c(1, Inf)])

  expect_identical(x_sparse[c(1, NaN)], x_dense[c(1, NaN)])
})

test_that("materialization works with sparse_logical()", {
  x_sparse <- sparse_logical(
    value = c(TRUE, NA, TRUE),
    position = c(1, 5, 8),
    10
  )
  x_dense <- c(TRUE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, TRUE, FALSE, FALSE)

  expect_identical(x_sparse[], x_dense)
})

test_that("sorting works with sparse_logical()", {
  x_sparse <- sparse_logical(logical(), integer(), 10)

  expect_true(is_sparse_logical(sort(x_sparse)))

  x_sparse <- sparse_logical(NA, 4, 10)

  expect_identical(
    sort(x_sparse),
    rep(FALSE, 9)
  )

  x_sparse <- sparse_logical(logical(), integer(), 10)

  expect_true(is_sparse_logical(sort(x_sparse)))

  x_sparse <- sparse_logical(c(TRUE, TRUE, TRUE), c(1, 4, 7), 7)

  expect_true(is_sparse_logical(sort(x_sparse)))

  x_sparse <- sparse_logical(c(TRUE, TRUE), c(1, 7), 7)

  expect_true(is_sparse_logical(sort(x_sparse)))

  x_sparse <- sparse_logical(c(TRUE, TRUE), c(1, 7), 7)

  expect_true(is_sparse_logical(sort(x_sparse)))
})

test_that("default argument is working", {
  expect_snapshot(
    error = TRUE,
    sparse_logical(TRUE, 1, 10, default = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    sparse_logical(c(TRUE, TRUE, NA), c(1, 4, 6), 10, default = TRUE)
  )

  x_sparse <- sparse_logical(
    value = c(FALSE, NA, FALSE),
    position = c(1, 5, 8),
    length = 10,
    default = TRUE
  )

  x_dense <- c(FALSE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, FALSE, TRUE, TRUE)

  for (i in seq_len(10)) {
    expect_identical(x_sparse[i], x_dense[i])
  }

  expect_identical(x_sparse[1:2], x_dense[1:2])

  expect_identical(x_sparse[3:7], x_dense[3:7])

  expect_identical(x_sparse[c(1, 5, 8, 1)], x_dense[c(1, 5, 8, 1)])

  expect_identical(x_sparse[], x_dense)
})

test_that("verbose testing", {
  withr::local_options("sparsevctrs.verbose_materialize" = TRUE)

  x <- sparse_logical(TRUE, 1, 1)
  expect_snapshot({
    tmp <- x[]
    tmp <- x[]
  })

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  x <- sparse_logical(TRUE, 1, 1)
  expect_snapshot({
    tmp <- x[]
    tmp <- x[]
  })

  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  x <- sparse_logical(TRUE, 1, 1)
  expect_snapshot(
    error = TRUE,
    {
      tmp <- x[]
    }
  )
})
