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
