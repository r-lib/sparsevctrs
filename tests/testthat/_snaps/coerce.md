# coerce_to_sparse_matrix() errors on wrong input

    Code
      coerce_to_sparse_matrix(1:10)
    Condition
      Error in `coerce_to_sparse_matrix()`:
      ! `x` must be a <data.frame>, not an integer vector.

---

    Code
      coerce_to_sparse_matrix(matrix(0, nrow = 10, ncol = 10))
    Condition
      Error in `coerce_to_sparse_matrix()`:
      ! `x` must be a <data.frame>, not a double matrix.

---

    Code
      coerce_to_sparse_matrix(iris)
    Condition
      Error in `coerce_to_sparse_matrix()`:
      x All columns of `x` must be numeric.
      i Non-numeric columns: Species.

# coerce_to_sparse_matrix() materializes non-zero defaulted columns

    Code
      res <- coerce_to_sparse_matrix(sparse_df)
    Output
      sparsevctrs: Sparse vector materialized
      sparsevctrs: Sparse vector materialized

