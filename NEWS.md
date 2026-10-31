# sparsevctrs (development version)

* Fixed bug where `sum()`, `min()`, `max()`, `anyNA()`, `is.unsorted()`, `[`, `sparse_values()`, `sparse_positions()`, and sparse multiplication on sparse vectors returned stale results after the vector was modified in place with `x[i] <- value`. (#147)

* Fixed bug where `is.unsorted()` on sparse character vectors compared string pointers instead of string contents, and ignored default values after the last non-default value.

* Fixed bug where `is.unsorted()` on sparse integer, sparse double, and sparse logical vectors ignored default values after the last non-default value, and returned `FALSE` for vectors with a single non-default value without comparing it against the default. (#133)

* Fixed bug where `min()` and `max()` on sparse integer and sparse double vectors included the default value even when every position was a non-default value. (#132)

* Fixed bug where `sum()` on sparse logical vectors used the wrong default value.

* `coerce_to_sparse_data_frame()` and `coerce_to_sparse_tibble()` are now much faster. (#66)

* `coerce_to_sparse_data_frame()` now also takes data frames and tibbles, turning each integer, double, logical, or character column into a sparse vector of the same type. (#126)

* `sparse_addition_scalar()`, `sparse_multiplication_scalar()` and `sparse_subtraction_scalar()` now give an error when `x` is not a sparse integer or double vector, instead of failing with an object not found error. (#136)

* `sparse_addition_scalar()`, `sparse_division_scalar()`, `sparse_multiplication_scalar()` and `sparse_subtraction_scalar()` are now faster. (#88)

* `sparse_addition_scalar()`, `sparse_multiplication_scalar()` and `sparse_subtraction_scalar()` now return `NA` with a warning, as dense integer vectors do, when a stored value of a sparse integer vector overflows, instead of failing with an error. (#88)

* `sparse_division_scalar()` and `sparse_multiplication_scalar()` now also scale the default value of the sparse vector, so they give the right result when the default is not zero. (#141)

* `sparse_division_scalar()` now gives an error when `x` is not a sparse integer or double vector, instead of silently accepting sparse logical vectors. (#88)

* `sparse_division_scalar()` now gives the same result as dense division when `val` is 0, with `-Inf` for negative values, `NaN` for zeros and `NA` for missing values, instead of `Inf` everywhere. (#146)

* `sparse_dummy()` is now faster, especially for factors with missing values and many levels. (#81)

* `sparse_dummy()` now returns missing values for missing elements of a factor with a single level and `one_hot = TRUE`, instead of 1.

* `sparse_multiplication()` no longer fails when one of two sparse inputs has no stored values. (#137)

* `sparse_multiplication()` no longer returns duplicated positions when a sparse double vector with a stored `NA` is multiplied by a dense double vector with a fractional value at the same position. (#139)

* `sparse_multiplication()` now returns `NA` with a warning, as dense integer vectors do, when the product of two integers overflows, instead of silently wrapping around. This also affects `sparse_mean()` with integer weights. (#150)

* `sparse_multiplication()` is much faster when the result has many stored values, since sorting them no longer takes quadratic time. With one million stored values on each side it now takes milliseconds instead of minutes. (#140)

* `sparse_multiplication_scalar()` now keeps stored `NA` values when multiplying by 0, since `NA * 0` is `NA`. Previously they were dropped. (#145)

* `sparse_replace_na()` now works with sparse logical vectors. (#136)

# sparsevctrs 0.3.6

* Replaced deprecated `ATTRIB()` macro with `R_altrep_class_name()` and `R_altrep_class_package()` for R 4.6.0 compatibility.

# sparsevctrs 0.3.5

* Fixed bug with sparse character vectors that occurred on R devel. (#122)

# sparsevctrs 0.3.4

## Bug Fixes

* Fixed bug where `sparse_multiplication()` had a stack imbalence when returning all 0s. (#113)

* Fixed bug where `sparse_is_na(type = "integer")` would error on character vectors. (#116)

# sparsevctrs 0.3.3

## Bug Fixes

* Fixed bug where `coerce_to_sparse_matrix()` would sometimes error if input had NA values. (#109)

# sparsevctrs 0.3.2

## Bug Fixes

* Fixed bug where `sparsity()` error on numeric vectors with classes. (#106)

# sparsevctrs 0.3.1

* Fixed bug where sparse multiplication caused undefined behaviour. ($103)

# sparsevctrs 0.3.0

## New Functions

* Adding the scalar arithmetic functions `sparse_division_scalar()`, `sparse_multiplication_scalar()`, `sparse_addition_scalar()`, `sparse_subtraction_scalar()`. (#87)

* Adding the arithmetic function `sparse_multiplication()`. (#93)

* Helper function `sparse_lag()` bas been added. (#99)

* Helper function `sparse_sqrt()` has been added. (#90)

* Helper function `sparse_replace_na()` has been added. (#91)

* Helper functions `sparse_is_na()` and `sparse_which_na()` have been added. (#92)

## Improvements

* Adding `wts` argument to `sparse_mean()`. (#95)

## Bug Fixes

* Fixed bug in `coerce_to_sparse_data_frame()` and `coerce_to_sparse_tibble()` where they didn't work with ngCMatrix. (#89)

# sparsevctrs 0.2.0

## New Functions

* `sparsity()` has been added, allows sparsity calculations of data.frames, matrices, and sparse matrices. (#82)

* Utility function `has_sparse_elements()` has been added. (#70)

* Helper function `sparse_dummy()` has beenn added. (#49)

* Helper functions `sparse_mean()`, `sparse_var()`, `sparse_sd()`, `sparse_median()` has been added. (#49)

## Improvements

* All sparse vector types now have a significant smaller base object size. (#67)

* All coerce functions have received a `call` argument. (#72)

* `is_sparse_vector()` has been rewritten for speed improvement. (#76)

* `coerce_to_sparse_matrix()` Now turns dense zeroes into sparse zeroes. (#77)

## Bug Fixes

* Fixed bug where `coerce_to_sparse_data_frame()` and `coerce_to_sparse_tibble()` didn't work with matrices with fully sparse columns. (#69)

* Fixed bugs where `coerce_to_sparse_matrix()` would error for completely sparse columns. (#77)

# sparsevctrs 0.1.0

* Initial CRAN submission.
