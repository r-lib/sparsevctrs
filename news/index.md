# Changelog

## sparsevctrs 0.3.5

- Fixed bug with sparse character vectors that occurred on R devel.
  ([\#122](https://github.com/r-lib/sparsevctrs/issues/122))

## sparsevctrs 0.3.4

CRAN release: 2025-05-25

### Bug Fixes

- Fixed bug where
  [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/reference/sparse-arithmatic.md)
  had a stack imbalence when returning all 0s.
  ([\#113](https://github.com/r-lib/sparsevctrs/issues/113))

- Fixed bug where `sparse_is_na(type = "integer")` would error on
  character vectors.
  ([\#116](https://github.com/r-lib/sparsevctrs/issues/116))

## sparsevctrs 0.3.3

CRAN release: 2025-04-14

### Bug Fixes

- Fixed bug where
  [`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_matrix.md)
  would sometimes error if input had NA values.
  ([\#109](https://github.com/r-lib/sparsevctrs/issues/109))

## sparsevctrs 0.3.2

CRAN release: 2025-03-21

### Bug Fixes

- Fixed bug where
  [`sparsity()`](https://r-lib.github.io/sparsevctrs/reference/sparsity.md)
  error on numeric vectors with classes.
  ([\#106](https://github.com/r-lib/sparsevctrs/issues/106))

## sparsevctrs 0.3.1

CRAN release: 2025-03-17

- Fixed bug where sparse multiplication caused undefined behaviour.
  (\$103)

## sparsevctrs 0.3.0

CRAN release: 2025-03-10

### New Functions

- Adding the scalar arithmatic functions
  [`sparse_division_scalar()`](https://r-lib.github.io/sparsevctrs/reference/sparse-arithmatic-scalar.md),
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/reference/sparse-arithmatic-scalar.md),
  [`sparse_addition_scalar()`](https://r-lib.github.io/sparsevctrs/reference/sparse-arithmatic-scalar.md),
  [`sparse_subtraction_scalar()`](https://r-lib.github.io/sparsevctrs/reference/sparse-arithmatic-scalar.md).
  ([\#87](https://github.com/r-lib/sparsevctrs/issues/87))

- Adding the arithmatic function
  [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/reference/sparse-arithmatic.md).
  ([\#93](https://github.com/r-lib/sparsevctrs/issues/93))

- Helper function
  [`sparse_lag()`](https://r-lib.github.io/sparsevctrs/reference/sparse_lag.md)
  bas been added.
  ([\#99](https://github.com/r-lib/sparsevctrs/issues/99))

- Helper function
  [`sparse_sqrt()`](https://r-lib.github.io/sparsevctrs/reference/sparse_sqrt.md)
  has been added.
  ([\#90](https://github.com/r-lib/sparsevctrs/issues/90))

- Helper function
  [`sparse_replace_na()`](https://r-lib.github.io/sparsevctrs/reference/sparse_replace_na.md)
  has been added.
  ([\#91](https://github.com/r-lib/sparsevctrs/issues/91))

- Helper functions
  [`sparse_is_na()`](https://r-lib.github.io/sparsevctrs/reference/sparse_is_na.md)
  and
  [`sparse_which_na()`](https://r-lib.github.io/sparsevctrs/reference/sparse_which_na.md)
  have been added.
  ([\#92](https://github.com/r-lib/sparsevctrs/issues/92))

### Improvements

- Adding `wts` argument to
  [`sparse_mean()`](https://r-lib.github.io/sparsevctrs/reference/sparse_mean.md).
  ([\#95](https://github.com/r-lib/sparsevctrs/issues/95))

### Bug Fixes

- Fixed bug in
  [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_data_frame.md)
  and
  [`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_tibble.md)
  where they didn’t work with ngCMatrix.
  ([\#89](https://github.com/r-lib/sparsevctrs/issues/89))

## sparsevctrs 0.2.0

CRAN release: 2025-01-22

### New Functions

- [`sparsity()`](https://r-lib.github.io/sparsevctrs/reference/sparsity.md)
  has been added, allows sparsity calculations of data.frames, matrices,
  and sparse matrices.
  ([\#82](https://github.com/r-lib/sparsevctrs/issues/82))

- Utility function
  [`has_sparse_elements()`](https://r-lib.github.io/sparsevctrs/reference/has_sparse_elements.md)
  has been added.
  ([\#70](https://github.com/r-lib/sparsevctrs/issues/70))

- Helper function
  [`sparse_dummy()`](https://r-lib.github.io/sparsevctrs/reference/sparse_dummy.md)
  has beenn added.
  ([\#49](https://github.com/r-lib/sparsevctrs/issues/49))

- Helper functions
  [`sparse_mean()`](https://r-lib.github.io/sparsevctrs/reference/sparse_mean.md),
  [`sparse_var()`](https://r-lib.github.io/sparsevctrs/reference/sparse_var.md),
  [`sparse_sd()`](https://r-lib.github.io/sparsevctrs/reference/sparse_sd.md),
  [`sparse_median()`](https://r-lib.github.io/sparsevctrs/reference/sparse_median.md)
  has been added.
  ([\#49](https://github.com/r-lib/sparsevctrs/issues/49))

### Improvements

- All sparse vector types now have a significant smaller base object
  size. ([\#67](https://github.com/r-lib/sparsevctrs/issues/67))

- All coerce functions have received a `call` argument.
  ([\#72](https://github.com/r-lib/sparsevctrs/issues/72))

- [`is_sparse_vector()`](https://r-lib.github.io/sparsevctrs/reference/type-predicates.md)
  has been rewritten for speed improvement.
  ([\#76](https://github.com/r-lib/sparsevctrs/issues/76))

- [`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_matrix.md)
  Now turns dense zeroes into sparse zeroes.
  ([\#77](https://github.com/r-lib/sparsevctrs/issues/77))

### Bug Fixes

- Fixed bug where
  [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_data_frame.md)
  and
  [`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_tibble.md)
  didn’t work with matrices with fully sparse columns.
  ([\#69](https://github.com/r-lib/sparsevctrs/issues/69))

- Fixed bugs where
  [`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_matrix.md)
  would error for completely sparse columns.
  ([\#77](https://github.com/r-lib/sparsevctrs/issues/77))

## sparsevctrs 0.1.0

CRAN release: 2024-05-31

- Initial CRAN submission.
