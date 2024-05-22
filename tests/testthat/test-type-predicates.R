test_that("is_sparse_vector works", {
  expect_true(is_sparse_vector(sparse_double(1, 1, 1)))
  expect_true(is_sparse_vector(sparse_integer(1, 1, 1)))

  expect_false(is_sparse_vector(c(1, 1, 1)))
  expect_false(is_sparse_vector(1:10))
  expect_false(is_sparse_vector(NULL))
})

test_that("is_sparse_numeric works", {
  expect_true(is_sparse_numeric(sparse_double(1, 1, 1)))
  expect_true(is_sparse_numeric(sparse_integer(1, 1, 1)))

  expect_false(is_sparse_numeric(c(1, 1, 1)))
  expect_false(is_sparse_numeric(1:10))
  expect_false(is_sparse_numeric(NULL))
})

test_that("is_sparse_double works", {
  expect_true(is_sparse_double(sparse_double(1, 1, 1)))

  expect_false(is_sparse_double(c(1, 1, 1)))
  expect_false(is_sparse_double(1:10))
  expect_false(is_sparse_double(NULL))
})

test_that("is_sparse_integer works", {
  expect_true(is_sparse_integer(sparse_integer(1, 1, 1)))

  expect_false(is_sparse_integer(c(1, 1, 1)))
  expect_false(is_sparse_integer(1:10))
  expect_false(is_sparse_integer(NULL))
})

test_that("is_sparse_character works", {
  expect_true(is_sparse_character(sparse_character("A", 1, 1)))

  expect_false(is_sparse_character(c(1, 1, 1)))
  expect_false(is_sparse_character(1:10))
  expect_false(is_sparse_character(NULL))
})

test_that("is_sparse_logical works", {
  expect_true(is_sparse_logical(sparse_logical(TRUE, 1, 1)))

  expect_false(is_sparse_logical(c(1, 1, 1)))
  expect_false(is_sparse_logical(1:10))
  expect_false(is_sparse_logical(NULL))
})
