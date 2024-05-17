# input checking is done correctly

    Code
      sparse_logical("1", 1, 1)
    Condition
      Error in `sparse_logical()`:
      ! `values` must be a logical vector, not a string.

---

    Code
      sparse_logical(1, 1, 1)
    Condition
      Error in `sparse_logical()`:
      ! `values` must be a logical vector, not a number.

---

    Code
      sparse_logical(NULL, 1, 1)
    Condition
      Error in `sparse_logical()`:
      ! `values` must be a logical vector, not NULL.

---

    Code
      sparse_logical(Inf, 1, 1)
    Condition
      Error in `sparse_logical()`:
      ! `values` must be a logical vector, not a number.

---

    Code
      sparse_logical(NaN, 1, 1)
    Condition
      Error in `sparse_logical()`:
      ! `values` must be a logical vector, not a numeric `NA`.

---

    Code
      sparse_logical(TRUE, 1.5, 1)
    Condition
      Error in `sparse_logical()`:
      x `positions` must contain integer values.
      i Non-integer values at index: 1.

---

    Code
      sparse_logical(TRUE, "1", 1)
    Condition
      Error in `sparse_logical()`:
      ! `positions` must be a integer vector, not a string.

---

    Code
      sparse_logical(TRUE, NULL, 1)
    Condition
      Error in `sparse_logical()`:
      ! `positions` must be a integer vector, not NULL.

---

    Code
      sparse_logical(TRUE, NA, 1)
    Condition
      Error in `sparse_logical()`:
      ! `positions` must be a integer vector, not `NA`.

---

    Code
      sparse_logical(TRUE, Inf, 1)
    Condition
      Error in `sparse_logical()`:
      x `positions` must not contain infinite values.
      i Infinite values at index: 1.

---

    Code
      sparse_logical(TRUE, NaN, 1)
    Condition
      Error in `sparse_logical()`:
      x `positions` must not contain NaN values.
      i NaN values at index: 1.

---

    Code
      sparse_logical(numeric(0), integer(0), -10)
    Condition
      Error in `sparse_logical()`:
      ! `length` must be a whole number larger than or equal to 0, not the number -10.

---

    Code
      sparse_logical(numeric(0), integer(0), 1e+10)
    Condition
      Error in `sparse_logical()`:
      ! `length` must be less than 2147483647, not 1e+10.

---

    Code
      sparse_logical(logical(0), integer(0), c(1, 10))
    Condition
      Error in `sparse_logical()`:
      ! `length` must be a whole number, not a double vector.

---

    Code
      sparse_logical(logical(0), integer(0), 1.5)
    Condition
      Error in `sparse_logical()`:
      ! `length` must be a whole number, not the number 1.5.

---

    Code
      sparse_logical(logical(0), integer(0), "1")
    Condition
      Error in `sparse_logical()`:
      ! `length` must be a whole number, not the string "1".

---

    Code
      sparse_logical(logical(0), integer(0), NA)
    Condition
      Error in `sparse_logical()`:
      ! `length` must be a whole number, not `NA`.

---

    Code
      sparse_logical(logical(0), integer(0), Inf)
    Condition
      Error in `sparse_logical()`:
      ! `length` must be a whole number, not `Inf`.

---

    Code
      sparse_logical(logical(0), integer(0), NULL)
    Condition
      Error in `sparse_logical()`:
      ! `length` must be a whole number, not `NULL`.

---

    Code
      sparse_logical(logical(0), integer(0), NaN)
    Condition
      Error in `sparse_logical()`:
      ! `length` must be a whole number, not `NaN`.

---

    Code
      sparse_logical(c(TRUE, TRUE), 1:6, 10)
    Condition
      Error in `sparse_logical()`:
      ! `value` (2) and `positions` (6) must have the same length.

---

    Code
      sparse_logical(TRUE, 1:6, 10)
    Condition
      Error in `sparse_logical()`:
      ! `value` (1) and `positions` (6) must have the same length.

---

    Code
      sparse_logical(c(TRUE, TRUE, TRUE, TRUE), c(1, 1, 5, 6), 10)
    Condition
      Error in `sparse_logical()`:
      x `positions` must not contain any duplicate values.
      i Duplicate values at index: 2.

---

    Code
      sparse_logical(rep(TRUE, 100), rep(1, 100), 100)
    Condition
      Error in `sparse_logical()`:
      x `positions` must not contain any duplicate values.
      i Duplicate values at index: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 99, and 100.

---

    Code
      sparse_logical(c(TRUE, TRUE), c(3, 1), 5)
    Condition
      Error in `sparse_logical()`:
      ! `positions` must be sorted in increasing order.

---

    Code
      sparse_logical(TRUE, 10, 5)
    Condition
      Error in `sparse_logical()`:
      x `positions` value must not be larger than `length`.
      i Offending values at index: 1.

---

    Code
      sparse_logical(rep(TRUE, 50), seq(25, 74), 50)
    Condition
      Error in `sparse_logical()`:
      x `positions` value must not be larger than `length`.
      i Offending values at index: 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, ..., 49, and 50.

---

    Code
      sparse_logical(TRUE, 0, 5)
    Condition
      Error in `sparse_logical()`:
      x `positions` value must positive.
      i Non-positive values at index: 1.

---

    Code
      sparse_logical(rep(TRUE, 101), seq(-50, 50), 100)
    Condition
      Error in `sparse_logical()`:
      x `positions` value must positive.
      i Non-positive values at index: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 50, and 51.

# default argument is working

    Code
      sparse_logical(TRUE, 1, 10, default = TRUE)
    Condition
      Error in `sparse_logical()`:
      x `values` value must not be equal to the default TRUE.
      i TRUE values at index: 1.

---

    Code
      sparse_logical(c(TRUE, TRUE, NA), c(1, 4, 6), 10, default = TRUE)
    Condition
      Error in `sparse_logical()`:
      x `values` value must not be equal to the default TRUE.
      i TRUE values at index: 1 and 2.

# verbose testing

    Code
      sparse_logical(TRUE, 1, 1)[]
    Output
      sparsevctrs: Sparse vector materialized
      [1] TRUE

