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

# coerce_to_sparse_matrix() can pass through error call

    Code
      tmp_fun(1)
    Condition
      Error in `tmp_fun()`:
      ! `x` must be a <data.frame>, not a number.

---

    Code
      tmp_fun(1)
    Condition
      Error in `tmp_fun()`:
      ! `x` must be a <sparseMatrix>, not a number.

# coerce_to_sparse_data_frame() errors with no column names

    Code
      coerce_to_sparse_data_frame(sparse_mat)
    Condition
      Error in `coerce_to_sparse_data_frame()`:
      ! `x` must have column names.

# coerce_to_sparse_data_frame() errors with unsupported columns

    Code
      coerce_to_sparse_data_frame(iris)
    Condition
      Error in `coerce_to_sparse_data_frame()`:
      x All columns of `x` must be integer, double, logical, or character vectors.
      i Unsupported columns: Species (factor).

---

    Code
      coerce_to_sparse_data_frame(data.frame(a = 1, b = Sys.Date(), c = factor("x")))
    Condition
      Error in `coerce_to_sparse_data_frame()`:
      x All columns of `x` must be integer, double, logical, or character vectors.
      i Unsupported columns: b (Date) and c (factor).

# coerce_to_sparse_data_frame() errors with wrong input

    Code
      coerce_to_sparse_data_frame(1:10)
    Condition
      Error in `coerce_to_sparse_data_frame()`:
      ! `x` must be a <sparseMatrix> or <data.frame>, not an integer vector.

---

    Code
      coerce_to_sparse_data_frame(matrix(0, nrow = 10, ncol = 10))
    Condition
      Error in `coerce_to_sparse_data_frame()`:
      ! `x` must be a <sparseMatrix> or <data.frame>, not a double matrix.

# coerce_to_sparse_data_frame() can pass through error call

    Code
      tmp_fun(1)
    Condition
      Error in `tmp_fun()`:
      ! `x` must be a <sparseMatrix> or <data.frame>, not a number.

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

