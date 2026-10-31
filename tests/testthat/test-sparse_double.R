test_that("input checking is done correctly", {
  expect_identical(
    sparse_double(1L, 1, 1),
    sparse_double(1, 1, 1)
  )

  # value
  expect_snapshot(
    error = TRUE,
    sparse_double("1", 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(NULL, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(Inf, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(NaN, 1, 1)
  )

  # position
  expect_snapshot(
    error = TRUE,
    sparse_double(1, 1.5, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(1, "1", 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(1, NULL, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(1, NA, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(1, Inf, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(1, NaN, 1)
  )

  # length
  expect_no_error(
    sparse_double(numeric(0), integer(0), 0)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), -10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), 10000000000)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), c(1, 10))
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), 1.5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), "1")
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), NA)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), Inf)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), NULL)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(numeric(0), integer(0), NaN)
  )

  # Length restriction
  expect_snapshot(
    error = TRUE,
    sparse_double(1:4, 1:6, 10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(1, 1:6, 10)
  )

  # duplicates in position
  expect_snapshot(
    error = TRUE,
    sparse_double(1:4, c(1, 1, 5, 6), 10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(1:100, rep(1, 100), 100)
  )

  # Ordered position
  expect_snapshot(
    error = TRUE,
    sparse_double(c(1, 2), c(3, 1), 5)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    sparse_double(1, 10, 5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(rep(1, 50), seq(25, 74), 50)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    sparse_double(1, 0, 5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(rep(1, 101), seq(-50, 50), 100)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    sparse_double(0, 1, 10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_double(rep(c(1, 0), 5), 1:10, 50)
  )
})

test_that("length() works with sparse_double()", {
  expect_identical(
    length(sparse_double(numeric(), integer(), 0)),
    0L
  )

  expect_identical(
    length(sparse_double(1, 1, 10)),
    10L
  )

  expect_identical(
    length(sparse_double(1, 1, 100)),
    100L
  )
})

test_that("single subsetting works with sparse_double()", {
  x_sparse <- sparse_double(value = c(10, NA, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10, 0, 0, 0, NA, 0, 0, 20, 0, 0)

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

test_that("subsetting one past the end works with sparse_double()", {
  x_sparse <- sparse_double(1, 3, 3, default = 7)
  x_dense <- c(7, 7, 1)

  expect_identical(x_sparse[4], x_dense[4])
  expect_identical(x_sparse[3:4], x_dense[3:4])
  expect_error(x_sparse[[4]], "subscript out of bounds")
  expect_error(x_dense[[4]], "subscript out of bounds")
})

test_that("multiple subsetting works with sparse_double()", {
  x_sparse <- sparse_double(value = c(10, NA, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10, 0, 0, 0, NA, 0, 0, 20, 0, 0)

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

test_that("materialization works with sparse_double()", {
  x_sparse <- sparse_double(value = c(10, NA, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10, 0, 0, 0, NA, 0, 0, 20, 0, 0)

  expect_identical(x_sparse[], x_dense)
})

test_that("sorting works with sparse_double()", {
  x_sparse <- sparse_double(numeric(), integer(), 10)

  expect_true(is_sparse_double(sort(x_sparse)))

  x_sparse <- sparse_double(NA, 4, 10)

  expect_identical(
    sort(x_sparse),
    rep(0, 9)
  )

  x_sparse <- sparse_double(numeric(), integer(), 10)

  expect_true(is_sparse_double(sort(x_sparse)))

  x_sparse <- sparse_double(c(1, 4, 5), c(1, 4, 7), 7)

  expect_false(is_sparse_double(sort(x_sparse)))

  x_sparse <- sparse_double(c(1, 5), c(1, 7), 7)

  expect_false(is_sparse_double(sort(x_sparse)))

  x_sparse <- sparse_double(c(-1, 5), c(1, 7), 7)

  expect_true(is_sparse_double(sort(x_sparse)))
})

test_that("is.unsorted() works with sparse_double()", {
  sorted <- sparse_double(c(1, 2), c(3, 4), 4)
  expect_identical(is.unsorted(sorted), is.unsorted(sorted[]))
  expect_false(is.unsorted(sorted))

  unsorted <- sparse_double(c(2, 1), c(2, 3), 4)
  expect_identical(is.unsorted(unsorted), is.unsorted(unsorted[]))
  expect_true(is.unsorted(unsorted))

  gap <- sparse_double(c(1, 2), c(1, 5), 5, default = 3)
  expect_identical(is.unsorted(gap), is.unsorted(gap[]))
  expect_true(is.unsorted(gap))

  trailing <- sparse_double(c(1, 5), c(1, 2), 10)
  expect_identical(is.unsorted(trailing), is.unsorted(trailing[]))
  expect_true(is.unsorted(trailing))

  single <- sparse_double(5, 1, 10)
  expect_identical(is.unsorted(single), is.unsorted(single[]))
  expect_true(is.unsorted(single))

  single <- sparse_double(-1, 10, 10)
  expect_identical(is.unsorted(single), is.unsorted(single[]))
  expect_true(is.unsorted(single))

  single <- sparse_double(1, 10, 10)
  expect_identical(is.unsorted(single), is.unsorted(single[]))
  expect_false(is.unsorted(single))
})

test_that("min method works with sparse_double()", {
  expect_snapshot(
    res <- min(sparse_double(integer(), integer(), 0))
  )
  expect_identical(res, Inf)

  expect_identical(
    min(sparse_double(numeric(), integer(), 1000000000)),
    0
  )

  expect_identical(
    min(sparse_double(-10, 10, 1000000000)),
    -10
  )

  expect_identical(
    min(sparse_double(-10, 10, 1000000000, default = -100)),
    -100
  )

  expect_identical(
    min(sparse_double(11:20, 11:20, 1000000000, default = 15.5)),
    11
  )

  expect_identical(
    min(sparse_double(NA, 10, 1000000000)),
    NA_real_
  )

  expect_identical(
    min(sparse_double(c(11:19, NA), 11:20, 1000000000)),
    NA_real_
  )

  expect_identical(
    min(sparse_double(NA, 10, 1000000000), na.rm = TRUE),
    0
  )

  expect_identical(
    min(sparse_double(c(-10, 11:19, NA), 10:20, 1000000000), na.rm = TRUE),
    -10
  )

  expect_identical(
    min(sparse_double(c(5, 6), 1:2, 2)),
    5
  )

  expect_snapshot(
    res <- min(sparse_double(NA, 1, 1), na.rm = TRUE)
  )
  expect_identical(res, Inf)
})

test_that("max method works with sparse_double()", {
  expect_snapshot(
    res <- max(sparse_double(integer(), integer(), 0))
  )

  expect_identical(
    max(sparse_double(numeric(), integer(), 1000000000)),
    0
  )

  expect_identical(
    max(sparse_double(10, 10, 1000000000)),
    10
  )

  expect_identical(
    max(sparse_double(10, 10, 1000000000, default = 100)),
    100
  )

  expect_identical(
    max(sparse_double(11:20, 11:20, 1000000000, default = 15.5)),
    20
  )

  expect_identical(
    max(sparse_double(NA, 10, 1000000000)),
    NA_real_
  )

  expect_identical(
    max(sparse_double(c(11:19, NA), 11:20, 1000000000)),
    NA_real_
  )

  expect_identical(
    max(sparse_double(NA, 10, 1000000000), na.rm = TRUE),
    0
  )

  expect_identical(
    max(sparse_double(c(-10, 11:19, NA), 10:20, 1000000000), na.rm = TRUE),
    19
  )

  expect_identical(
    max(sparse_double(c(-5, -6), 1:2, 2)),
    -5
  )

  expect_snapshot(
    res <- max(sparse_double(NA, 1, 1), na.rm = TRUE)
  )
  expect_identical(res, -Inf)
})

test_that("anyNA method works with sparse_double", {
  expect_false(
    anyNA(sparse_double(numeric(), integer(), 1000000000))
  )

  expect_false(
    anyNA(sparse_double(1, 1, 1000000000))
  )

  expect_true(
    anyNA(sparse_double(NA, 1, 1000000000))
  )

  expect_true(
    anyNA(sparse_double(c(-10, 11:19, NA), 10:20, 1000000000))
  )
})

test_that("sum method works with sparse_double", {
  expect_identical(
    sum(sparse_double(numeric(), integer(), 0)),
    0
  )

  expect_identical(
    sum(sparse_double(numeric(), integer(), 1000000000)),
    0
  )

  expect_identical(
    sum(sparse_double(numeric(), integer(), 1000000000, default = 0.0001)),
    100000
  )

  expect_identical(
    sum(sparse_double(c(1, 5.4, 10), c(1, 5, 10), 10)),
    16.4
  )

  expect_identical(
    sum(sparse_double(c(1, 5.4, 10), c(1, 5, 10), 10, default = -1)),
    16.4 - 7
  )

  expect_identical(
    sum(sparse_double(c(1, 5.4, NA), c(1, 5, 10), 10)),
    NA_real_
  )

  expect_identical(
    sum(sparse_double(c(1, 5.4, NA), c(1, 5, 10), 10), na.rm = TRUE),
    6.4
  )
})

test_that("methods work after in-place modification of sparse_double", {
  x <- sparse_double(c(1, NA), c(1, 2), 5)
  x[is.na(x)] <- 0
  expect_identical(sum(x), 1)
  expect_true(!anyNA(x))

  x <- sparse_double(c(1, 2), c(1, 2), 5)
  x[3] <- 100
  expect_identical(max(x), 100)
  expect_identical(min(x), 0)
  expect_identical(x[1:5], c(1, 2, 100, 0, 0))
  expect_true(is.unsorted(x))

  x <- sparse_double(c(1, 2), c(1, 2), 5)
  x[3] <- NA
  expect_true(anyNA(x))
  expect_identical(sum(x), NA_real_)
})

test_that("sparse_multiplication() works after in-place modification", {
  x <- sparse_double(c(1, 2), c(1, 2), 4)
  y <- sparse_double(c(3, 4), c(2, 3), 4)
  x[3] <- 5
  y[4] <- 6

  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    c(1, 2, 5, 0) * c(0, 3, 4, 6)
  )
  expect_identical(
    sparse_multiplication(x, c(1, 1, 1, 1)) |> as.numeric(),
    c(1, 2, 5, 0)
  )
})

test_that("sparse_multiplication() treats non-sparse ALTREP as dense", {
  x <- sort(c(2, 1, 3:10))
  expect_true(is_altrep_non_sparse_vector(x))
  y <- sparse_double(c(1, 2), c(1, 2), 10)
  expect_identical(
    sparse_multiplication(y, x) |> as.numeric(),
    c(1, 4, rep(0, 8))
  )
  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    c(1, 4, rep(0, 8))
  )
  expect_identical(
    sparse_multiplication(sparse_double(2, 2, 3), sort(c(3, 1, 2))) |>
      as.numeric(),
    c(0, 4, 0)
  )
  expect_identical(
    sparse_multiplication(
      sparse_double(2, 1, 3),
      sort(c(3, 1, 2), decreasing = TRUE)
    ) |>
      as.numeric(),
    c(6, 0, 0)
  )
})

test_that("sparse_multiplication() treats non-sparse integer ALTREP as dense", {
  x <- sort(c(3L, 1L, 2L))
  y <- sparse_integer(2L, 2, 3)
  expect_identical(sparse_multiplication(y, x) |> as.integer(), c(0L, 4L, 0L))
  expect_identical(
    sparse_multiplication(sort(c(3L, 1L, 2L)), x) |> as.integer(),
    c(1L, 4L, 9L)
  )
})

test_that("sparse_multiplication() handles more non-sparse ALTREP inputs", {
  expect_identical(
    sparse_multiplication(sort(c(3, 1, 2)), sort(c(3, 1, 2))) |> as.numeric(),
    c(1, 4, 9)
  )

  expect_identical(
    sparse_multiplication(sparse_double(2, 2, 3), sort(c(3L, 1L, 2L))) |>
      as.numeric(),
    c(0, 4, 0)
  )
  expect_identical(
    sparse_multiplication(sort(c(3L, 1L, 2L)), sparse_double(2, 2, 3)) |>
      as.numeric(),
    c(0, 4, 0)
  )

  df <- data.frame(w = sort(c(3, 1, 2)))
  expect_identical(
    sparse_multiplication(sparse_double(2, 2, 3), df$w) |> as.numeric(),
    c(0, 4, 0)
  )

  expect_identical(
    sparse_multiplication(
      sparse_double(2, 2, 3, default = 1),
      sort(c(3, 1, 2))
    ) |>
      as.numeric(),
    c(1, 4, 3)
  )

  expect_identical(
    sparse_multiplication(sparse_double(2, 2, 3), sparse_double(5, 2, 3)) |>
      as.numeric(),
    c(0, 10, 0)
  )
})

test_that("sparse_multiplication() keeps 0 * non-finite from dense operand", {
  x <- sparse_double(c(2, 3), c(1, 2), 4)
  y <- c(1, 1, Inf, NaN)
  expected <- c(2, 3, 0, 0) * y

  res <- sparse_multiplication(x, y)
  expect_true(is_sparse_double(res))
  expect_identical(as.numeric(res), expected)
  expect_identical(sparse_multiplication(y, x) |> as.numeric(), expected)

  y <- c(1, 1, -Inf, NA)
  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    c(2, 3, 0, 0) * y
  )
})

test_that("sparse_multiplication() keeps 0 * non-finite with empty sparse", {
  x <- sparse_double(numeric(), integer(), 4L)
  y <- c(1, 1, Inf, NaN)

  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    c(0, 0, 0, 0) * y
  )
  expect_identical(
    sparse_multiplication(y, x) |> as.numeric(),
    c(0, 0, 0, 0) * y
  )
})

test_that("sparse_multiplication() handles non-finite at stored positions", {
  x <- sparse_double(c(2, 3), c(1, 2), 4)
  y <- c(Inf, NaN, 1, Inf)

  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    c(2, 3, 0, 0) * y
  )
})

test_that("sparse_multiplication() mixes stored NA with non-finite", {
  x <- sparse_double(c(NA, 3), c(1, 2), 4)
  y <- c(0, 1, Inf, 0)

  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    c(NA, 3, 0, 0) * y
  )
})

test_that("sparse_multiplication() keeps stored non-finite times 0", {
  x <- sparse_double(c(2, 3), c(1, 2), 5)
  x[3] <- Inf
  x[4] <- NaN
  x_dense <- c(2, 3, Inf, NaN, 0)

  zero <- c(0, 0, 0, 0, 0)
  expect_identical(
    sparse_multiplication(x, zero) |> as.numeric(),
    x_dense * zero
  )
  expect_identical(
    sparse_multiplication(zero, x) |> as.numeric(),
    x_dense * zero
  )

  y <- sparse_double(5, 1, 5)
  y_dense <- c(5, 0, 0, 0, 0)
  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    x_dense * y_dense
  )
  expect_identical(
    sparse_multiplication(y, x) |> as.numeric(),
    x_dense * y_dense
  )

  y <- sparse_double(c(5, 7), c(1, 3), 5)
  y_dense <- c(5, 0, 7, 0, 0)
  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    x_dense * y_dense
  )
})

test_that("sparse_multiplication() keeps NA and stored non-finite apart", {
  x <- sparse_double(c(NA, 3), c(1, 2), 4)
  x[3] <- Inf
  x_dense <- c(NA, 3, Inf, 0)
  y <- sparse_double(4, 4, 4)

  expect_identical(
    sparse_multiplication(x, c(0, 0, 0, 0)) |> as.numeric(),
    x_dense * c(0, 0, 0, 0)
  )
  expect_identical(
    sparse_multiplication(x, y) |> as.numeric(),
    x_dense * c(0, 0, 0, 4)
  )
})

test_that("sparse_multiplication_scalar() keeps stored non-finite times 0", {
  x <- sparse_double(c(2, NA), c(1, 2), 5)
  x[3] <- Inf
  x[4] <- NaN
  x_dense <- c(2, NA, Inf, NaN, 0)

  res <- sparse_multiplication_scalar(x, 0)
  expect_true(is_sparse_double(res))
  expect_identical(as.numeric(res), x_dense * 0)
  expect_identical(sparse_positions(res), c(2L, 3L, 4L))
})

test_that("sparse_mean() works with non-sparse ALTREP weights", {
  x <- sparse_double(c(1, 2), 1:2, 6)
  w <- c(3, 1, 2, 6, 5, 4)
  expect_equal(
    sparse_mean(x, wts = sort(w)),
    weighted.mean(c(1, 2, 0, 0, 0, 0), sort(w))
  )
})

test_that("repeated reads after one modification stay correct", {
  x <- sparse_double(1, 1, 5)
  x[2] <- 3
  expect_identical(sum(x), 4)
  expect_identical(sum(x), 4)
  x[3] <- 2
  expect_identical(sum(x), 6)
})

test_that("default argument is working", {
  expect_snapshot(
    error = TRUE,
    sparse_double(1, 1, 10, default = 1:10)
  )

  expect_snapshot(
    error = TRUE,
    sparse_double(1, 1, 10, default = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    sparse_double(c(1, 1, 4), c(1, 4, 6), 10, default = 1)
  )

  expect_snapshot(
    error = TRUE,
    sparse_double(1, 1, 10, default = Inf)
  )

  expect_snapshot(
    error = TRUE,
    sparse_double(1, 1, 10, default = -Inf)
  )

  expect_snapshot(
    error = TRUE,
    sparse_double(1, 1, 10, default = NaN)
  )

  x_sparse <- sparse_double(
    value = c(10, NA, 20),
    position = c(1, 5, 8),
    length = 10,
    default = 4
  )

  x_dense <- c(10, 4, 4, 4, NA, 4, 4, 20, 4, 4)

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

  x <- sparse_double(1, 1, 1)
  expect_snapshot({
    tmp <- sparse_double_materialize(x)
    tmp <- sparse_double_materialize(x)
  })

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  x <- sparse_double(1, 1, 1)
  expect_snapshot({
    tmp <- sparse_double_materialize(x)
    tmp <- sparse_double_materialize(x)
  })

  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  x <- sparse_double(1, 1, 1)
  expect_snapshot(
    error = TRUE,
    {
      tmp <- sparse_double_materialize(x)
    }
  )
})

test_that("printing works #48", {
  expect_snapshot(
    sparse_double(1, 1, 10) + 1
  )
})

test_that("in-place modification works with non-zero defaults and resets", {
  x <- sparse_double(c(1, 2), c(1, 2), 5, default = 3)
  x[4] <- 7
  expect_identical(sum(x), 1 + 2 + 3 + 7 + 3)
  expect_identical(max(x), 7)
  expect_identical(min(x), 1)
  expect_identical(sparse_default(x), 3)

  x <- sparse_double(c(1, 2), c(1, 2), 5, default = 3)
  x[1:2] <- 3
  expect_identical(sum(x), 15)
  expect_identical(max(x), 3)
  expect_identical(min(x), 3)
  expect_identical(x[1:5], rep(3, 5))

  x <- sparse_double(c(1, 2), c(1, 2), 5)
  x[1] <- 0
  expect_identical(sum(x), 2)
  expect_identical(x[1:5], c(0, 2, 0, 0, 0))
})

test_that("in-place modification works with NA and NaN", {
  x <- sparse_double(c(1, 2), c(1, 2), 5)
  x[3] <- NaN
  expect_identical(x[1:5], c(1, 2, NaN, 0, 0))
  expect_identical(sparse_values(x), c(1, 2, NaN))
  expect_identical(sum(x), NaN)

  x <- sparse_double(c(1, 2), c(1, 2), 5)
  x[3] <- NA
  expect_identical(sparse_values(x), c(1, 2, NA))
  expect_identical(sum(x, na.rm = TRUE), 3)
})

test_that("in-place modification works with length 0 vectors", {
  x <- sparse_double(numeric(), integer(), 0)
  x[integer()] <- numeric()
  expect_identical(sum(x), 0)
  expect_identical(sparse_values(x), numeric())
  expect_identical(sparse_positions(x), integer())
})

test_that("in-place modification does not affect copies", {
  x <- sparse_double(c(1, 2), c(1, 2), 5)
  y <- x
  x[3] <- 9

  expect_identical(sum(y), 3)
  expect_identical(sparse_values(y), c(1, 2))
  expect_identical(sum(x), 12)
})

test_that("methods are unchanged by materialization without modification", {
  x <- sparse_double(c(1, 2), c(1, 2), 5)
  x[]

  expect_identical(sparse_values(x), c(1, 2))
  expect_identical(sparse_positions(x), 1:2)
  expect_identical(sum(x), 3)
  expect_identical(max(x), 2)
  expect_identical(x[2:3], c(2, 0))
})
