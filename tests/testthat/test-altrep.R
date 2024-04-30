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

  # Length restriction
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1:4, 1:6, 10)
  )
  expect_snapshot(
    error = TRUE,
    new_sparse_real(1, 1:6, 10)
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

test_that("subsetting works with new_sparse_real()", {
  x_sparse <- new_sparse_real(value = c(10, 13, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10, 0, 0, 0, 13, 0, 0, 20, 0, 0)

  for (i in seq_len(10)) {
    expect_identical(x_sparse[i], x_dense[i])
  }

  expect_identical(x_sparse[1:2], x_dense[1:2])

  expect_identical(x_sparse[3:7], x_dense[3:7])

  expect_identical(x_sparse[-5], x_dense[-5])

  expect_identical(x_sparse[-c(5:7)], x_dense[-c(5:7)])

  # testing outside range returns NA
  # expect_identical(x_sparse[c(1, 11)], x_dense[c(1, 11)])
})

test_that("materialization works with new_sparse_real()", {
  x_sparse <- new_sparse_real(value = c(10, 13, 20), position = c(1, 5, 8), 10)
  x_dense <- c(10, 0, 0, 0, 13, 0, 0, 20, 0, 0)

  expect_identical(x_sparse[], x_dense)
})