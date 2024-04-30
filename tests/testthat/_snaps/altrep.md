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

