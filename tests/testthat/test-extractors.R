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

test_that("extractors reflect in-place modification", {
  x <- sparse_double(c(1, 2), c(1, 2), 5)
  x[3] <- 100
  expect_identical(sparse_positions(x), 1:3)
  expect_identical(sparse_values(x), c(1, 2, 100))
  expect_identical(sparse_default(x), 0)

  x <- sparse_integer(c(1L, 2L), c(1, 2), 5)
  x[1] <- 0L
  expect_identical(sparse_positions(x), 1:2)
  expect_identical(sparse_values(x), c(0L, 2L))

  x <- sparse_logical(TRUE, 1, 4)
  x[3] <- NA
  expect_identical(sparse_positions(x), c(1L, 3L))
  expect_identical(sparse_values(x), c(TRUE, NA))

  x <- sparse_character("a", 1, 4)
  x[3] <- "z"
  expect_identical(sparse_positions(x), c(1L, 3L))
  expect_identical(sparse_values(x), c("a", "z"))
})
