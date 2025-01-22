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
