# Changelog

## sparsevctrs (development version)

## sparsevctrs 0.4.0

CRAN release: 2026-11-01

### Improvements

- [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_data_frame.md)
  now also takes data frames and tibbles, turning each integer, double,
  logical, or character column into a sparse vector of the same type.
  ([\#126](https://github.com/r-lib/sparsevctrs/issues/126))

- [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_data_frame.md)
  and
  [`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_tibble.md)
  are now much faster.
  ([\#66](https://github.com/r-lib/sparsevctrs/issues/66))

- [`sparse_addition_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_division_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  and
  [`sparse_subtraction_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  are now faster.
  ([\#88](https://github.com/r-lib/sparsevctrs/issues/88),
  [\#147](https://github.com/r-lib/sparsevctrs/issues/147))

- [`sparse_dummy()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_dummy.md)
  is now faster, especially for factors with missing values and many
  levels. ([\#81](https://github.com/r-lib/sparsevctrs/issues/81))

- [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic.md)
  is now much faster when the result has many stored values, since
  sorting them no longer takes quadratic time. With one million stored
  values on each side it now takes milliseconds instead of minutes.
  ([\#140](https://github.com/r-lib/sparsevctrs/issues/140))

- [`sparse_replace_na()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_replace_na.md)
  now works with sparse logical vectors.
  ([\#136](https://github.com/r-lib/sparsevctrs/issues/136))

### Bug Fixes

- [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_data_frame.md)
  and
  [`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_tibble.md)
  now give an error when a sparse matrix contains `NaN` or infinite
  values, as they already do for data frames, instead of creating sparse
  vectors that break [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html).
  ([\#156](https://github.com/r-lib/sparsevctrs/issues/156))

- [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_data_frame.md)
  and
  [`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_tibble.md)
  now work with logical sparse matrices, giving sparse double columns,
  instead of failing with an error about `REAL()`.
  ([\#162](https://github.com/r-lib/sparsevctrs/issues/162))

- [`is.unsorted()`](https://rdrr.io/r/base/is.unsorted.html) now
  compares the contents of strings, not string pointers, for sparse
  character vectors. It also takes default values after the last
  non-default value into account.
  ([\#131](https://github.com/r-lib/sparsevctrs/issues/131))

- [`is.unsorted()`](https://rdrr.io/r/base/is.unsorted.html) now takes
  default values after the last non-default value into account for
  sparse integer, double, and logical vectors, and compares a single
  non-default value against the default.
  ([\#133](https://github.com/r-lib/sparsevctrs/issues/133))

- [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) now ignore the default
  value of sparse integer and double vectors when every position is a
  non-default value.
  ([\#132](https://github.com/r-lib/sparsevctrs/issues/132))

- [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) with `na.rm = TRUE`
  now return `Inf` and `-Inf` with a warning for sparse integer and
  double vectors with no non-missing values, as dense vectors do.
  ([\#159](https://github.com/r-lib/sparsevctrs/issues/159))

- [`sparse_addition_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  and
  [`sparse_subtraction_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  now give an error when `x` is not a sparse integer or double vector,
  instead of failing with an object not found error.
  ([\#136](https://github.com/r-lib/sparsevctrs/issues/136))

- [`sparse_addition_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  and
  [`sparse_subtraction_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  now always give an error when `x` is a sparse integer vector and `val`
  is not a whole number, instead of only when a result is not a whole
  number. ([\#161](https://github.com/r-lib/sparsevctrs/issues/161))

- [`sparse_addition_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_division_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  and
  [`sparse_subtraction_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  now return `NaN` instead of `NA` when `val` is `NaN`, and no longer
  store values that are equal to the new default value after underflow
  or rounding.
  ([\#161](https://github.com/r-lib/sparsevctrs/issues/161))

- [`sparse_addition_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  and
  [`sparse_subtraction_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  now return `NA` with a warning, as dense integer vectors do, when a
  stored value of a sparse integer vector overflows, instead of failing
  with an error.
  ([\#88](https://github.com/r-lib/sparsevctrs/issues/88))

- [`sparse_division_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  and
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  now also scale the default value of the sparse vector, so they give
  the right result when the default is not zero.
  ([\#141](https://github.com/r-lib/sparsevctrs/issues/141))

- [`sparse_division_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  now gives an error when `x` is not a sparse integer or double vector,
  instead of silently accepting sparse logical vectors.
  ([\#88](https://github.com/r-lib/sparsevctrs/issues/88))

- [`sparse_division_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  now gives the same result as dense division when `val` is 0, with
  `-Inf` for negative values, `NaN` for zeros and `NA` for missing
  values, instead of `Inf` everywhere.
  ([\#146](https://github.com/r-lib/sparsevctrs/issues/146))

- [`sparse_double()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_double.md)
  now gives an error when `default` is `NaN`, `Inf` or `-Inf`, since
  they made [`sum()`](https://rdrr.io/r/base/sum.html) return `NaN`.
  ([\#160](https://github.com/r-lib/sparsevctrs/issues/160))

- [`sparse_dummy()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_dummy.md)
  now returns missing values for missing elements of a factor with a
  single level and `one_hot = TRUE`, instead of 1.
  ([\#155](https://github.com/r-lib/sparsevctrs/issues/155))

- [`sparse_mean()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_mean.md)
  and
  [`sparse_median()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_median.md)
  now return `NaN` and `NA` respectively for length-0 vectors, instead
  of the default.
  ([\#158](https://github.com/r-lib/sparsevctrs/issues/158))

- [`sparse_mean()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_mean.md),
  [`sparse_var()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_var.md)
  and
  [`sparse_sd()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_sd.md)
  now work for sparse integer vectors with a large default and length,
  instead of returning `NA` with an overflow warning.
  ([\#157](https://github.com/r-lib/sparsevctrs/issues/157))

- [`sparse_mean()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_mean.md)
  with `wts` summing to zero now returns `NaN`, as
  [`weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html) does.
  ([\#157](https://github.com/r-lib/sparsevctrs/issues/157))

- [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic.md)
  now works when one of two sparse inputs has no stored values.
  ([\#137](https://github.com/r-lib/sparsevctrs/issues/137))

- [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic.md)
  no longer returns duplicated positions when a sparse double vector
  with a stored `NA` is multiplied by a dense double vector with a
  fractional value at the same position.
  ([\#139](https://github.com/r-lib/sparsevctrs/issues/139))

- [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic.md)
  now returns `NA` with a warning, as dense integer vectors do, when the
  product of two integers overflows, instead of silently wrapping
  around. This also affects
  [`sparse_mean()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_mean.md)
  with integer weights.
  ([\#150](https://github.com/r-lib/sparsevctrs/issues/150))

- [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic.md)
  now returns `NaN` where a sparse vector has its default value of 0 and
  the dense vector has `Inf`, `-Inf` or `NaN`, since `0 * Inf` is `NaN`.
  Previously these positions were dropped and gave 0.
  ([\#152](https://github.com/r-lib/sparsevctrs/issues/152))

- [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic.md)
  and
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  with `val = 0` now return `NaN` for stored `Inf` and `NaN` values of a
  sparse double vector multiplied by 0, as dense multiplication does.
  Previously `Inf` was dropped and gave 0, and `NaN` was returned as
  `NA`. ([\#153](https://github.com/r-lib/sparsevctrs/issues/153))

- [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md)
  now keeps stored `NA` values when multiplying by 0, since `NA * 0` is
  `NA`. Previously they were dropped.
  ([\#145](https://github.com/r-lib/sparsevctrs/issues/145))

- [`sparse_var()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_var.md)
  and
  [`sparse_sd()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_sd.md)
  now work for non-sparse input, and return `NA` when fewer than 2
  non-missing observations remain, as
  [`var()`](https://rdrr.io/r/stats/cor.html) does.
  ([\#158](https://github.com/r-lib/sparsevctrs/issues/158))

- [`sum()`](https://rdrr.io/r/base/sum.html) now uses the right default
  value for sparse logical vectors.
  ([\#131](https://github.com/r-lib/sparsevctrs/issues/131))

- [`sum()`](https://rdrr.io/r/base/sum.html) now returns a double when
  the sum of a sparse integer vector is outside the integer range, as
  [`sum()`](https://rdrr.io/r/base/sum.html) does for dense integer
  vectors in current R versions, instead of overflowing silently.
  ([\#151](https://github.com/r-lib/sparsevctrs/issues/151))

- [`sum()`](https://rdrr.io/r/base/sum.html),
  [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html),
  [`anyNA()`](https://rdrr.io/r/base/NA.html),
  [`is.unsorted()`](https://rdrr.io/r/base/is.unsorted.html), `[`,
  [`sparse_values()`](https://r-lib.github.io/sparsevctrs/dev/reference/extractors.md),
  [`sparse_positions()`](https://r-lib.github.io/sparsevctrs/dev/reference/extractors.md),
  and sparse multiplication now give up-to-date results for sparse
  vectors that were modified in place with `x[i] <- value`.
  ([\#148](https://github.com/r-lib/sparsevctrs/issues/148))

## sparsevctrs 0.3.6

CRAN release: 2026-01-27

- Replaced deprecated `ATTRIB()` macro with `R_altrep_class_name()` and
  `R_altrep_class_package()` for R 4.6.0 compatibility.

## sparsevctrs 0.3.5

CRAN release: 2025-12-09

- Fixed bug with sparse character vectors that occurred on R devel.
  ([\#122](https://github.com/r-lib/sparsevctrs/issues/122))

## sparsevctrs 0.3.4

CRAN release: 2025-05-25

### Bug Fixes

- Fixed bug where
  [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic.md)
  had a stack imbalence when returning all 0s.
  ([\#113](https://github.com/r-lib/sparsevctrs/issues/113))

- Fixed bug where `sparse_is_na(type = "integer")` would error on
  character vectors.
  ([\#116](https://github.com/r-lib/sparsevctrs/issues/116))

## sparsevctrs 0.3.3

CRAN release: 2025-04-14

### Bug Fixes

- Fixed bug where
  [`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_matrix.md)
  would sometimes error if input had NA values.
  ([\#109](https://github.com/r-lib/sparsevctrs/issues/109))

## sparsevctrs 0.3.2

CRAN release: 2025-03-21

### Bug Fixes

- Fixed bug where
  [`sparsity()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparsity.md)
  error on numeric vectors with classes.
  ([\#106](https://github.com/r-lib/sparsevctrs/issues/106))

## sparsevctrs 0.3.1

CRAN release: 2025-03-17

- Fixed bug where sparse multiplication caused undefined behaviour.
  (\$103)

## sparsevctrs 0.3.0

CRAN release: 2025-03-10

### New Functions

- Adding the scalar arithmetic functions
  [`sparse_division_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_multiplication_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_addition_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md),
  [`sparse_subtraction_scalar()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic-scalar.md).
  ([\#87](https://github.com/r-lib/sparsevctrs/issues/87))

- Adding the arithmetic function
  [`sparse_multiplication()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse-arithmetic.md).
  ([\#93](https://github.com/r-lib/sparsevctrs/issues/93))

- Helper function
  [`sparse_lag()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_lag.md)
  bas been added.
  ([\#99](https://github.com/r-lib/sparsevctrs/issues/99))

- Helper function
  [`sparse_sqrt()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_sqrt.md)
  has been added.
  ([\#90](https://github.com/r-lib/sparsevctrs/issues/90))

- Helper function
  [`sparse_replace_na()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_replace_na.md)
  has been added.
  ([\#91](https://github.com/r-lib/sparsevctrs/issues/91))

- Helper functions
  [`sparse_is_na()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_is_na.md)
  and
  [`sparse_which_na()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_which_na.md)
  have been added.
  ([\#92](https://github.com/r-lib/sparsevctrs/issues/92))

### Improvements

- Adding `wts` argument to
  [`sparse_mean()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_mean.md).
  ([\#95](https://github.com/r-lib/sparsevctrs/issues/95))

### Bug Fixes

- Fixed bug in
  [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_data_frame.md)
  and
  [`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_tibble.md)
  where they didn’t work with ngCMatrix.
  ([\#89](https://github.com/r-lib/sparsevctrs/issues/89))

## sparsevctrs 0.2.0

CRAN release: 2025-01-22

### New Functions

- [`sparsity()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparsity.md)
  has been added, allows sparsity calculations of data.frames, matrices,
  and sparse matrices.
  ([\#82](https://github.com/r-lib/sparsevctrs/issues/82))

- Utility function
  [`has_sparse_elements()`](https://r-lib.github.io/sparsevctrs/dev/reference/has_sparse_elements.md)
  has been added.
  ([\#70](https://github.com/r-lib/sparsevctrs/issues/70))

- Helper function
  [`sparse_dummy()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_dummy.md)
  has beenn added.
  ([\#49](https://github.com/r-lib/sparsevctrs/issues/49))

- Helper functions
  [`sparse_mean()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_mean.md),
  [`sparse_var()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_var.md),
  [`sparse_sd()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_sd.md),
  [`sparse_median()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_median.md)
  has been added.
  ([\#49](https://github.com/r-lib/sparsevctrs/issues/49))

### Improvements

- All sparse vector types now have a significant smaller base object
  size. ([\#67](https://github.com/r-lib/sparsevctrs/issues/67))

- All coerce functions have received a `call` argument.
  ([\#72](https://github.com/r-lib/sparsevctrs/issues/72))

- [`is_sparse_vector()`](https://r-lib.github.io/sparsevctrs/dev/reference/type-predicates.md)
  has been rewritten for speed improvement.
  ([\#76](https://github.com/r-lib/sparsevctrs/issues/76))

- [`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_matrix.md)
  Now turns dense zeroes into sparse zeroes.
  ([\#77](https://github.com/r-lib/sparsevctrs/issues/77))

### Bug Fixes

- Fixed bug where
  [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_data_frame.md)
  and
  [`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_tibble.md)
  didn’t work with matrices with fully sparse columns.
  ([\#69](https://github.com/r-lib/sparsevctrs/issues/69))

- Fixed bugs where
  [`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_matrix.md)
  would error for completely sparse columns.
  ([\#77](https://github.com/r-lib/sparsevctrs/issues/77))

## sparsevctrs 0.1.0

CRAN release: 2024-05-31

- Initial CRAN submission.
