test_that(".positions works", {
  expect_identical(
    .positions(new_sparse_real(1, 5, 10)),
    5L
  )

  expect_identical(
    .positions(new_sparse_real(1:3, 5:7, 10)),
    5:7
  )
})

test_that(".positions works", {
  expect_identical(
    .values(new_sparse_real(1, 5, 10)),
    1
  )

  expect_identical(
    .values(new_sparse_real(1:3, 5:7, 10)),
    c(1, 2, 3)
  )
})
