# input checking is done correctly

    Code
      sparse_integer("1", 1, 1)
    Condition
      Error in `sparse_integer()`:
      ! Can't convert `values` <character> to <integer>.

---

    Code
      sparse_integer(0.5, 1, 1)
    Condition
      Error in `sparse_integer()`:
      ! Can't convert from `values` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      sparse_integer(NULL, 1, 1)
    Condition
      Error in `sparse_integer()`:
      ! `value` (0) and `positions` (1) must have the same length.

---

    Code
      sparse_integer(Inf, 1, 1)
    Condition
      Error in `sparse_integer()`:
      ! Can't convert from `values` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      sparse_integer(NaN, 1, 1)
    Condition
      Error in `sparse_integer()`:
      x `values` must not contain NaN values.
      i NaN values at index: 1.

---

    Code
      sparse_integer(1, 1.5, 1)
    Condition
      Error in `sparse_integer()`:
      x `positions` must contain integer values.
      i Non-integer values at index: 1.

---

    Code
      sparse_integer(1, "1", 1)
    Condition
      Error in `sparse_integer()`:
      ! `positions` must be a integer vector, not a string.

---

    Code
      sparse_integer(1, NULL, 1)
    Condition
      Error in `sparse_integer()`:
      ! `positions` must be a integer vector, not NULL.

---

    Code
      sparse_integer(1, NA, 1)
    Condition
      Error in `sparse_integer()`:
      ! `positions` must be a integer vector, not `NA`.

---

    Code
      sparse_integer(1, Inf, 1)
    Condition
      Error in `sparse_integer()`:
      x `positions` must not contain infinite values.
      i Infinite values at index: 1.

---

    Code
      sparse_integer(1, NaN, 1)
    Condition
      Error in `sparse_integer()`:
      x `positions` must not contain NaN values.
      i NaN values at index: 1.

---

    Code
      sparse_integer(numeric(0), integer(0), -10)
    Condition
      Error in `sparse_integer()`:
      ! `length` must be a whole number larger than or equal to 0, not the number -10.

---

    Code
      sparse_integer(numeric(0), integer(0), 1e+10)
    Condition
      Error in `sparse_integer()`:
      ! `length` must be less than 2147483647, not 1e+10.

---

    Code
      sparse_integer(integer(0), integer(0), c(1, 10))
    Condition
      Error in `sparse_integer()`:
      ! `length` must be a whole number, not a double vector.

---

    Code
      sparse_integer(integer(0), integer(0), 1.5)
    Condition
      Error in `sparse_integer()`:
      ! `length` must be a whole number, not the number 1.5.

---

    Code
      sparse_integer(integer(0), integer(0), "1")
    Condition
      Error in `sparse_integer()`:
      ! `length` must be a whole number, not the string "1".

---

    Code
      sparse_integer(integer(0), integer(0), NA)
    Condition
      Error in `sparse_integer()`:
      ! `length` must be a whole number, not `NA`.

---

    Code
      sparse_integer(integer(0), integer(0), Inf)
    Condition
      Error in `sparse_integer()`:
      ! `length` must be a whole number, not `Inf`.

---

    Code
      sparse_integer(integer(0), integer(0), NULL)
    Condition
      Error in `sparse_integer()`:
      ! `length` must be a whole number, not `NULL`.

---

    Code
      sparse_integer(integer(0), integer(0), NaN)
    Condition
      Error in `sparse_integer()`:
      ! `length` must be a whole number, not `NaN`.

---

    Code
      sparse_integer(1:4, 1:6, 10)
    Condition
      Error in `sparse_integer()`:
      ! `value` (4) and `positions` (6) must have the same length.

---

    Code
      sparse_integer(1, 1:6, 10)
    Condition
      Error in `sparse_integer()`:
      ! `value` (1) and `positions` (6) must have the same length.

---

    Code
      sparse_integer(1:4, c(1, 1, 5, 6), 10)
    Condition
      Error in `sparse_integer()`:
      x `positions` must not contain any duplicate values.
      i Duplicate values at index: 2.

---

    Code
      sparse_integer(1:100, rep(1, 100), 100)
    Condition
      Error in `sparse_integer()`:
      x `positions` must not contain any duplicate values.
      i Duplicate values at index: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 99, and 100.

---

    Code
      sparse_integer(c(1, 2), c(3, 1), 5)
    Condition
      Error in `sparse_integer()`:
      ! `positions` must be sorted in increasing order.

---

    Code
      sparse_integer(1, 10, 5)
    Condition
      Error in `sparse_integer()`:
      x `positions` value must not be larger than `length`.
      i Offending values at index: 1.

---

    Code
      sparse_integer(rep(1, 50), seq(25, 74), 50)
    Condition
      Error in `sparse_integer()`:
      x `positions` value must not be larger than `length`.
      i Offending values at index: 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, ..., 49, and 50.

---

    Code
      sparse_integer(1, 0, 5)
    Condition
      Error in `sparse_integer()`:
      x `positions` value must positive.
      i Non-positive values at index: 1.

---

    Code
      sparse_integer(rep(1, 101), seq(-50, 50), 100)
    Condition
      Error in `sparse_integer()`:
      x `positions` value must positive.
      i Non-positive values at index: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 50, and 51.

---

    Code
      sparse_integer(0, 1, 10)
    Condition
      Error in `sparse_integer()`:
      x `values` value must not be equal to the default 0.
      i 0 values at index: 1.

---

    Code
      sparse_integer(rep(c(1, 0), 5), 1:10, 50)
    Condition
      Error in `sparse_integer()`:
      x `values` value must not be equal to the default 0.
      i 0 values at index: 2, 4, 6, 8, and 10.

# min method works with sparse_integer()

    Code
      res <- min(sparse_integer(integer(), integer(), 0))
    Condition
      Warning:
      no non-missing arguments to min; returning Inf

# max method works with sparse_integer()

    Code
      res <- max(sparse_integer(integer(), integer(), 0))
    Condition
      Warning:
      no non-missing arguments to max; returning -Inf

# default argument is working

    Code
      sparse_integer(1, 1, 10, default = 1:10)
    Condition
      Error in `sparse_integer()`:
      ! `default` must be a whole number, not an integer vector.

---

    Code
      sparse_integer(1, 1, 10, default = TRUE)
    Condition
      Error in `sparse_integer()`:
      ! `default` must be a whole number, not `TRUE`.

---

    Code
      sparse_integer(c(1, 1, 4), c(1, 4, 6), 10, default = 1)
    Condition
      Error in `sparse_integer()`:
      x `values` value must not be equal to the default 1.
      i 1 values at index: 1 and 2.

# verbose testing

    Code
      sparse_integer(1, 1, 1)[]
    Output
      sparsevctrs: Sparse vector materialized
      [1] 1

# printing works #48

    Code
      sparse_integer(1, 1, 10) + 1
    Output
       [1] 2 1 1 1 1 1 1 1 1 1

