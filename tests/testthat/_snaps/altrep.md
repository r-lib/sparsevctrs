# input checking is done correctly

    Code
      new_sparse_real("1", 1, 1)
    Condition
      Error in `new_sparse_real()`:
      ! `value` must be a numeric vector, not a string.

---

    Code
      new_sparse_real(NULL, 1, 1)
    Condition
      Error in `new_sparse_real()`:
      ! `value` must be a numeric vector, not NULL.

---

    Code
      new_sparse_real(1, 1.5, 1)
    Condition
      Error in `new_sparse_real()`:
      ! Can't convert from `position` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      new_sparse_real(1, "1", 1)
    Condition
      Error in `new_sparse_real()`:
      ! `position` must be a integer vector, not a number.

---

    Code
      new_sparse_real(1, NULL, 1)
    Condition
      Error in `new_sparse_real()`:
      ! `position` must be a integer vector, not a number.

---

    Code
      new_sparse_real(numeric(0), integer(0), c(1, 10))
    Condition
      Error in `new_sparse_real()`:
      ! `length` must be a whole number, not a double vector.

---

    Code
      new_sparse_real(numeric(0), integer(0), 1.5)
    Condition
      Error in `new_sparse_real()`:
      ! `length` must be a whole number, not the number 1.5.

---

    Code
      new_sparse_real(numeric(0), integer(0), "1")
    Condition
      Error in `new_sparse_real()`:
      ! `length` must be a whole number, not the string "1".

---

    Code
      new_sparse_real(numeric(0), integer(0), NA)
    Condition
      Error in `new_sparse_real()`:
      ! `length` must be a whole number, not `NA`.

---

    Code
      new_sparse_real(numeric(0), integer(0), Inf)
    Condition
      Error in `new_sparse_real()`:
      ! `length` must be a whole number, not `Inf`.

---

    Code
      new_sparse_real(numeric(0), integer(0), NULL)
    Condition
      Error in `new_sparse_real()`:
      ! `length` must be a whole number, not `NULL`.

---

    Code
      new_sparse_real(1:4, 1:6, 10)
    Condition
      Error in `new_sparse_real()`:
      ! `value` (4) and `position` (6) must have the same length.

---

    Code
      new_sparse_real(1, 1:6, 10)
    Condition
      Error in `new_sparse_real()`:
      ! `value` (1) and `position` (6) must have the same length.

---

    Code
      new_sparse_real(1:4, c(1, 1, 5, 6), 10)
    Condition
      Error in `new_sparse_real()`:
      x `position` must not contain any duplicate values.
      i Duplicate values at index: 2.

---

    Code
      new_sparse_real(1:100, rep(1, 100), 100)
    Condition
      Error in `new_sparse_real()`:
      x `position` must not contain any duplicate values.
      i Duplicate values at index: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 99, and 100.

---

    Code
      new_sparse_real(c(1, 2), c(3, 1), 5)
    Condition
      Error in `new_sparse_real()`:
      ! `position` must be sorted in increasing order.

---

    Code
      new_sparse_real(1, 10, 5)
    Condition
      Error in `new_sparse_real()`:
      x `position` value must not be larger than `length`.
      i Offending values at index: 1.

---

    Code
      new_sparse_real(rep(1, 50), seq(25, 74), 50)
    Condition
      Error in `new_sparse_real()`:
      x `position` value must not be larger than `length`.
      i Offending values at index: 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, ..., 49, and 50.

---

    Code
      new_sparse_real(1, 0, 5)
    Condition
      Error in `new_sparse_real()`:
      x `position` value must positive.
      i Non-positive values at index: 1.

---

    Code
      new_sparse_real(rep(1, 101), seq(-50, 50), 100)
    Condition
      Error in `new_sparse_real()`:
      x `position` value must positive.
      i Non-positive values at index: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 50, and 51.

---

    Code
      new_sparse_real(0, 1, 10)
    Condition
      Error in `new_sparse_real()`:
      x `value` value must not be 0.
      i 0 values at index: 1.

---

    Code
      new_sparse_real(rep(c(1, 0), 5), 1:10, 50)
    Condition
      Error in `new_sparse_real()`:
      x `value` value must not be 0.
      i 0 values at index: 2, 4, 6, 8, and 10.

