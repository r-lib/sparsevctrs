test_that("sparse_positions works with altrep_sparse_double", {
  expect_identical(
    sparse_positions(sparse_double(1, 5, 10)),
    5L
  )

  expect_identical(
    sparse_positions(sparse_double(1:3, 5:7, 10)),
    5:7
  )
})

test_that("sparse_positions works with numeric vectors", {
  expect_identical(
    sparse_positions(c(1, 6, 4, 2)),
    seq_len(4)
  )

  expect_identical(
    sparse_positions(101:200),
    1:100
  )
})

test_that("sparse_values works with altrep_sparse_double", {
  expect_identical(
    sparse_values(sparse_double(1, 5, 10)),
    1
  )

  expect_identical(
    sparse_values(sparse_double(1:3, 5:7, 10)),
    c(1, 2, 3)
  )
})

test_that("sparse_values works with numeric vectors", {
  expect_identical(
    sparse_values(c(1, 6, 4, 2)),
    c(1, 6, 4, 2)
  )

  expect_identical(
    sparse_values(101:200),
    101:200
  )
})

test_that("sparse_default works with altrep_sparse_double", {
  expect_identical(
    sparse_default(sparse_double(1, 5, 10)),
    0
  )

  expect_identical(
    sparse_default(sparse_double(1, 5, 10, default = 11)),
    11
  )
})

test_that("sparse_values works with numeric vectors", {
  expect_identical(
    sparse_default(c(1, 6, 4, 2)),
    NA
  )
})
