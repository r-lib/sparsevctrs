test_that("sparse_dummy() works", {
  x <- factor(c("a", "b", "c", "d", "a"))
  exp <- list(
    a = sparse_integer(c(1, 1), c(1, 5), 5),
    b = sparse_integer(1, 2, 5),
    c = sparse_integer(1, 3, 5),
    d = sparse_integer(1, 4, 5)
  )

  res <- sparse_dummy(x) 
  expect_identical(
    res,
    exp
  )

  expect_true(
    all(vapply(res, is_sparse_integer, logical(1)))
  )
})
 
test_that("sparse_dummy() errors with wrong input", {
  expect_snapshot(
    error = TRUE,
    sparse_dummy(letters)
  )
  expect_snapshot(
    error = TRUE,
    sparse_dummy(mtcars)
  )
  expect_snapshot(
    error = TRUE,
    sparse_dummy(1:5)
  )
  expect_snapshot(
    error = TRUE,
    sparse_dummy(NULL)
  )
})

