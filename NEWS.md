# sparsevctrs (development version)

* All sparse vector types now have a significant smaller base object size. (#67)

* Fixed bug where `coerce_to_sparse_data_frame()` and `coerce_to_sparse_tibble()` didn't work with matrices with fully sparse columns. (#69)

* All coerce functions have received a `call` argument. (#72)

* Helper function `has_sparse_elements()` has been added (#70)

* `is_sparse_vector()` has been rewritten for speed improvement. (#76)

# sparsevctrs 0.1.0

* Initial CRAN submission.
