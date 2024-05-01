test_that("input checking is done correctly", {
  expect_identical(
    new_sparse_real(1L, 1, 1),
    new_sparse_real(1, 1, 1)
  )

  # value
  expect_snapshot(
    error = TRUE,
    new_sparse_real("1", 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(NULL, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(NA, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(Inf, 1, 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(NaN, 1, 1)
  )

  # position
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, 1.5, 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, "1", 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, NULL, 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, NA, 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, Inf, 1)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, NaN, 1)
  )

  # length
  expect_no_error(
    new_sparse_real(numeric(0), integer(0), 0)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(numeric(0), integer(0), c(1, 10))
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(numeric(0), integer(0), 1.5)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(numeric(0), integer(0), "1")
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(numeric(0), integer(0), NA)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(numeric(0), integer(0), Inf)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(numeric(0), integer(0), NULL)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(numeric(0), integer(0), NaN)
  )


  # Length restriction
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1:4, 1:6, 10)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, 1:6, 10)
  )

  # duplicates in position
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1:4, c(1, 1, 5, 6), 10)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1:100, rep(1, 100), 100)
  )
  
  # Ordered position
  expect_snapshot(
    error = TRUE,
    new_sparse_real(c(1, 2), c(3, 1), 5)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, 10, 5)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(rep(1, 50), seq(25, 74), 50)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, 0, 5)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(rep(1, 101), seq(-50, 50), 100)
  )

  # Too large position values
  expect_snapshot(
    error = TRUE,
    new_sparse_real(0, 1, 10)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(rep(c(1, 0), 5), 1:10, 50)
  )
})

test_that("length() works with new_sparse_real()", {
  expect_identical(
    length(new_sparse_real(numeric(), integer(), 0)),
    0L
  )

  expect_identical(
    length(new_sparse_real(1, 1, 10)),
    10L
  )

  expect_identical(
    length(new_sparse_real(1, 1, 100)),
    100L
  )
})

test_that("single subsetting works with new_sparse_real()", {
  x_sparse <- new_sparse_real(value = c(10, 13, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10, 0, 0, 0, 13, 0, 0, 20, 0, 0)

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

test_that("multiple subsetting works with new_sparse_real()", {
  x_sparse <- new_sparse_real(value = c(10, 13, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10, 0, 0, 0, 13, 0, 0, 20, 0, 0)

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

test_that("materialization works with new_sparse_real()", {
  x_sparse <- new_sparse_real(value = c(10, 13, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10, 0, 0, 0, 13, 0, 0, 20, 0, 0)

  expect_identical(x_sparse[], x_dense)
})


test_that("is_sparse_vector works", {
  expect_true(is_sparse_vector(new_sparse_real(1, 1, 1)))

  expect_false(is_sparse_vector(c(1, 1, 1)))
  expect_false(is_sparse_vector(1:10))
  expect_false(is_sparse_vector(NULL))
})

test_that("verbose testing", {
  withr::local_options("sparsevctrs.verbose_materialize" = TRUE)

  expect_snapshot(
    new_sparse_real(1, 1, 1)[]
  )
})
