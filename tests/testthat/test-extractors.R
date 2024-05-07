test_that(".positions works with altrep_sparse_real", {
  expect_identical(
    .positions(new_sparse_double(1, 5, 10)),
    5L
  )

  expect_identical(
    .positions(new_sparse_double(1:3, 5:7, 10)),
    5:7
  )
})

test_that(".positions works with numeric vectors", {
  expect_identical(
    .positions(c(1, 6, 4, 2)),
    seq_len(4)
  )

  expect_identical(
    .positions(101:200),
    1:100
  )
})

test_that(".values works with altrep_sparse_real", {
  expect_identical(
    .values(new_sparse_double(1, 5, 10)),
    1
  )

  expect_identical(
    .values(new_sparse_double(1:3, 5:7, 10)),
    c(1, 2, 3)
  )
})

test_that(".values works with numeric vectors", {
  expect_identical(
    .values(c(1, 6, 4, 2)),
    c(1, 6, 4, 2)
  )

  expect_identical(
    .values(101:200),
    101:200
  )
})
