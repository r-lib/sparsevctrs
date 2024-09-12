test_that("input checking is done correctly", {
  expect_identical(
    sparse_integer(1L, 1, 1),
    sparse_integer(1, 1, 1)
  )

  # value
  expect_snapshot(
    error = TRUE,
    sparse_integer("1", 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(0.5, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(NULL, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(Inf, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(NaN, 1, 1)
  )

  # position
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, 1.5, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, "1", 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, NULL, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, NA, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, Inf, 1)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, NaN, 1)
  )

  # length
  expect_no_error(
    sparse_integer(integer(0), integer(0), 0)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(numeric(0), integer(0), -10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(numeric(0), integer(0), 10000000000)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(integer(0), integer(0), c(1, 10))
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(integer(0), integer(0), 1.5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(integer(0), integer(0), "1")
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(integer(0), integer(0), NA)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(integer(0), integer(0), Inf)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(integer(0), integer(0), NULL)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(integer(0), integer(0), NaN)
  )


  # Length restriction
  expect_snapshot(
    error = TRUE,
    sparse_integer(1:4, 1:6, 10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, 1:6, 10)
  )

  # duplicates in position
  expect_snapshot(
    error = TRUE,
    sparse_integer(1:4, c(1, 1, 5, 6), 10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(1:100, rep(1, 100), 100)
  )
  
  # Ordered position
  expect_snapshot(
    error = TRUE,
    sparse_integer(c(1, 2), c(3, 1), 5)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, 10, 5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(rep(1, 50), seq(25, 74), 50)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, 0, 5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(rep(1, 101), seq(-50, 50), 100)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    sparse_integer(0, 1, 10)
  )
  expect_snapshot(
    error = TRUE,
    sparse_integer(rep(c(1, 0), 5), 1:10, 50)
  )
})

test_that("length() works with sparse_integer()", {
  expect_identical(
    length(sparse_integer(integer(), integer(), 0)),
    0L
  )

  expect_identical(
    length(sparse_integer(1, 1, 10)),
    10L
  )

  expect_identical(
    length(sparse_integer(1, 1, 100)),
    100L
  )
})

test_that("single subsetting works with sparse_integer()", {
  x_sparse <- sparse_integer(value = c(10, NA, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10L, 0L, 0L, 0L, NA, 0L, 0L, 20L, 0L, 0L)

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

test_that("multiple subsetting works with sparse_integer()", {
  x_sparse <- sparse_integer(value = c(10, NA, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10L, 0L, 0L, 0L, NA, 0L, 0L, 20L, 0L, 0L)

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

test_that("materialization works with sparse_integer()", {
  x_sparse <- sparse_integer(value = c(10, NA, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10L, 0L, 0L, 0L, NA, 0L, 0L, 20L, 0L, 0L)

  expect_identical(x_sparse[], x_dense)
})

test_that("sorting works with sparse_integer()", {
  x_sparse <- sparse_integer(integer(), integer(), 10)

  expect_true(is_sparse_integer(sort(x_sparse)))

  x_sparse <- sparse_integer(NA, 4, 10)

  expect_identical(
    sort(x_sparse),
    rep(0L, 9)
  )

  x_sparse <- sparse_integer(integer(), integer(), 10)

  expect_true(is_sparse_integer(sort(x_sparse)))

  x_sparse <- sparse_integer(c(1, 4, 5), c(1, 4, 7), 7)

  expect_false(is_sparse_integer(sort(x_sparse)))

  x_sparse <- sparse_integer(c(1, 5), c(1, 7), 7)

  expect_false(is_sparse_integer(sort(x_sparse)))

  x_sparse <- sparse_integer(c(-1, 5), c(1, 7), 7)

  expect_true(is_sparse_integer(sort(x_sparse)))
})

test_that("min method works with sparse_integer()", {

  expect_snapshot(
    res <- min(sparse_integer(integer(), integer(), 0))
  )
  expect_identical(res, Inf)

  expect_identical(
    min(sparse_integer(integer(), integer(), 1000000000)),
    0L
  )

  expect_identical(
    min(sparse_integer(-10, 10, 1000000000)),
    -10L
  )

  expect_identical(
    min(sparse_integer(-10, 10, 1000000000, default = -100)),
    -100L
  )

  expect_identical(
    min(sparse_integer(c(10:14, 16:20), 11:20, 1000000000, default = 15L)),
    10L
  )

  expect_identical(
    min(sparse_integer(NA, 10, 1000000000)),
    NA_integer_
  )

  expect_identical(
    min(sparse_integer(c(11:19, NA), 11:20, 1000000000)),
    NA_integer_
  )

  expect_identical(
    min(sparse_integer(NA, 10, 1000000000), na.rm = TRUE),
    0L
  )

  expect_identical(
    min(sparse_integer(c(-10, 11:19, NA), 10:20, 1000000000), na.rm = TRUE),
    -10L
  )
})

test_that("max method works with sparse_integer()", {
  expect_snapshot(
    res <- max(sparse_integer(integer(), integer(), 0))
  )
  expect_identical(res, -Inf)

  expect_identical(
    max(sparse_integer(integer(), integer(), 1000000000)),
    0L
  )

  expect_identical(
    max(sparse_integer(10, 10, 1000000000)),
    10L
  )

  expect_identical(
    max(sparse_integer(10, 10, 1000000000, default = 100)),
    100L
  )

  expect_identical(
    max(sparse_integer(c(10:14, 16:20), 11:20, 1000000000, default = 15)),
    20L
  )

  expect_identical(
    max(sparse_integer(NA, 10, 1000000000)),
    NA_integer_
  )

  expect_identical(
    max(sparse_integer(c(11:19, NA), 11:20, 1000000000)),
    NA_integer_
  )

  expect_identical(
    max(sparse_integer(NA, 10, 1000000000), na.rm = TRUE),
    0L
  )

  expect_identical(
    max(sparse_integer(c(-10, 11:19, NA), 10:20, 1000000000), na.rm = TRUE),
    19L
  )
})

test_that("anyNA method works with sparse_integer", {
  expect_false(
    anyNA(sparse_integer(integer(), integer(), 1000000000))
  )

  expect_false(
    anyNA(sparse_integer(1, 1, 1000000000))
  )

  expect_true(
    anyNA(sparse_integer(NA, 1, 1000000000))
  )

  expect_true(
    anyNA(sparse_integer(c(-10, 11:19, NA), 10:20, 1000000000))
  )
})

test_that("sum method works with sparse_integer", {
  expect_identical(
    sum(sparse_integer(integer(), integer(), 0)),
    0L
  )

  expect_identical(
    sum(sparse_integer(integer(), integer(), 1000000000)),
    0L
  )

  expect_identical(
    sum(sparse_integer(integer(), integer(), 1000000000, default = 2)),
    2000000000L
  )

  expect_identical(
    sum(sparse_integer(c(1, 54, 10), c(1, 5, 10), 10)),
    65L
  )

  expect_identical(
    sum(sparse_integer(c(1, 54, 10), c(1, 5, 10), 10, default = -1)),
    58L
  )

  expect_identical(
    sum(sparse_integer(c(1, 54, NA), c(1, 5, 10), 10)),
    NA_integer_
  )

  expect_identical(
    sum(sparse_integer(c(1, 54, NA), c(1, 5, 10), 10), na.rm = TRUE),
    55L
  )
})

test_that("default argument is working", {
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, 1, 10, default = 1:10)
  )
  
  expect_snapshot(
    error = TRUE,
    sparse_integer(1, 1, 10, default = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    sparse_integer(c(1, 1, 4), c(1, 4, 6), 10, default = 1)
  )

  x_sparse <- sparse_integer(
    value = c(10, NA, 20), 
    position = c(1, 5, 8), 
    length = 10, 
    default = 4
  )

  x_dense <- c(10L, 4L, 4L, 4L, NA, 4L, 4L, 20L, 4L, 4L)

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

  x <- sparse_integer(1, 1, 1)
  expect_snapshot({
    tmp <- x[]
    tmp <- x[]
  })

  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  x <- sparse_integer(1, 1, 1)
  expect_snapshot({
    tmp <- x[]
    tmp <- x[]
  })

  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  x <- sparse_integer(1, 1, 1)
  expect_snapshot(
    error = TRUE,
    {
      tmp <- x[]
    }
  )
})

test_that("printing works #48", {
  expect_snapshot(
    sparse_integer(1, 1, 10) + 1
  )
})

