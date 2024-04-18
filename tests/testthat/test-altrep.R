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

  expect_identical(x_sparse[11], NA_real_)
})
