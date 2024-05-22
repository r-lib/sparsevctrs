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

# coerce_to_sparse_data_frame() errors with no column names

    Code
      coerce_to_sparse_data_frame(sparse_mat)
    Condition
      Error in `coerce_to_sparse_data_frame()`:
      ! `x` must have column names.

# coerce_to_sparse_data_frame() errors with wrong input

    Code
      coerce_to_sparse_data_frame(mtcars)
    Condition
      Error in `coerce_to_sparse_data_frame()`:
      ! `x` must be a <sparseMatrix>, not a data frame.

---

    Code
      coerce_to_sparse_data_frame(1:10)
    Condition
      Error in `coerce_to_sparse_data_frame()`:
      ! `x` must be a <sparseMatrix>, not an integer vector.

# coerce_to_sparse_tibble() errors with no column names

    Code
      coerce_to_sparse_tibble(sparse_mat)
    Condition
      Error in `coerce_to_sparse_tibble()`:
      ! `x` must have column names.

# coerce_to_sparse_tibble() errors with wrong input

    Code
      coerce_to_sparse_tibble(mtcars)
    Condition
      Error in `coerce_to_sparse_tibble()`:
      ! `x` must be a <sparseMatrix>, not a data frame.

---

    Code
      coerce_to_sparse_tibble(1:10)
    Condition
      Error in `coerce_to_sparse_tibble()`:
      ! `x` must be a <sparseMatrix>, not an integer vector.

