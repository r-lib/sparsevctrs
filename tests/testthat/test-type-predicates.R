test_that("is_sparse_vector works", {
  expect_true(is_sparse_vector(sparse_double(1, 1, 1)))

  expect_false(is_sparse_vector(c(1, 1, 1)))
  expect_false(is_sparse_vector(1:10))
  expect_false(is_sparse_vector(NULL))
})

test_that("is_sparse_double works", {
  expect_true(is_sparse_double(sparse_double(1, 1, 1)))

  expect_false(is_sparse_double(c(1, 1, 1)))
  expect_false(is_sparse_double(1:10))
  expect_false(is_sparse_double(NULL))
})
