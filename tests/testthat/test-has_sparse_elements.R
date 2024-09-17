test_that("has_sparse_elements() works", {
  expect_false(has_sparse_elements(mtcars))

  mtcars$sparse <- sparse_integer(1, 1, 32)

  expect_true(has_sparse_elements(mtcars))
})
