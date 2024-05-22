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

