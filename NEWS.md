# sparsevctrs 0.3.1

* Fixed bug where sparse multiplication caused undefined behaviour. ($103)

# sparsevctrs 0.3.0

## New Functions

* Adding the scalar arithmatic functions `sparse_division_scalar()`, `sparse_multiplication_scalar()`, `sparse_addition_scalar()`, `sparse_subtraction_scalar()`. (#87)

* Adding the arithmatic function `sparse_multiplication()`. (#93)

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
