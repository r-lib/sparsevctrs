# input checking is done correctly

    Code
      sparse_character(1, 1, 1)
    Condition
      Error in `sparse_character()`:
      ! Can't convert `values` <double> to <character>.

---

    Code
      sparse_character(0.5, 1, 1)
    Condition
      Error in `sparse_character()`:
      ! Can't convert `values` <double> to <character>.

---

    Code
      sparse_character(NULL, 1, 1)
    Condition
      Error in `sparse_character()`:
      ! `value` (0) and `positions` (1) must have the same length.

---

    Code
      sparse_character(Inf, 1, 1)
    Condition
      Error in `sparse_character()`:
      ! Can't convert `values` <double> to <character>.

---

    Code
      sparse_character(NaN, 1, 1)
    Condition
      Error in `sparse_character()`:
      ! Can't convert `values` <double> to <character>.

---

    Code
      sparse_character("A", 1.5, 1)
    Condition
      Error in `sparse_character()`:
      x `positions` must contain integer values.
      i Non-integer values at index: 1.

---

    Code
      sparse_character("A", "1", 1)
    Condition
      Error in `sparse_character()`:
      ! `positions` must be a integer vector, not a string.

---

    Code
      sparse_character("A", NULL, 1)
    Condition
      Error in `sparse_character()`:
      ! `positions` must be a integer vector, not NULL.

---

    Code
      sparse_character("A", NA, 1)
    Condition
      Error in `sparse_character()`:
      ! `positions` must be a integer vector, not `NA`.

---

    Code
      sparse_character("A", Inf, 1)
    Condition
      Error in `sparse_character()`:
      x `positions` must not contain infinite values.
      i Infinite values at index: 1.

---

    Code
      sparse_character("A", NaN, 1)
    Condition
      Error in `sparse_character()`:
      x `positions` must not contain NaN values.
      i NaN values at index: 1.

---

    Code
      sparse_character(numeric(0), integer(0), -10)
    Condition
      Error in `sparse_character()`:
      ! `length` must be a whole number larger than or equal to 0, not the number -10.

---

    Code
      sparse_character(numeric(0), integer(0), 1e+10)
    Condition
      Error in `sparse_character()`:
      ! `length` must be less than 2147483647, not 1e+10.

---

    Code
      sparse_character(character(0), integer(0), c(1, 10))
    Condition
      Error in `sparse_character()`:
      ! `length` must be a whole number, not a double vector.

---

    Code
      sparse_character(character(0), integer(0), 1.5)
    Condition
      Error in `sparse_character()`:
      ! `length` must be a whole number, not the number 1.5.

---

    Code
      sparse_character(character(0), integer(0), "1")
    Condition
      Error in `sparse_character()`:
      ! `length` must be a whole number, not the string "1".

---

    Code
      sparse_character(character(0), integer(0), NA)
    Condition
      Error in `sparse_character()`:
      ! `length` must be a whole number, not `NA`.

---

    Code
      sparse_character(character(0), integer(0), Inf)
    Condition
      Error in `sparse_character()`:
      ! `length` must be a whole number, not `Inf`.

---

    Code
      sparse_character(character(0), integer(0), NULL)
    Condition
      Error in `sparse_character()`:
      ! `length` must be a whole number, not `NULL`.

---

    Code
      sparse_character(character(0), integer(0), NaN)
    Condition
      Error in `sparse_character()`:
      ! `length` must be a whole number, not `NaN`.

---

    Code
      sparse_character(letters[1:4], 1:6, 10)
    Condition
      Error in `sparse_character()`:
      ! `value` (4) and `positions` (6) must have the same length.

---

    Code
      sparse_character("A", 1:6, 10)
    Condition
      Error in `sparse_character()`:
      ! `value` (1) and `positions` (6) must have the same length.

---

    Code
      sparse_character(letters[1:4], c(1, 1, 5, 6), 10)
    Condition
      Error in `sparse_character()`:
      x `positions` must not contain any duplicate values.
      i Duplicate values at index: 2.

---

    Code
      sparse_character(letters, rep(1, 26), 100)
    Condition
      Error in `sparse_character()`:
      x `positions` must not contain any duplicate values.
      i Duplicate values at index: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 25, and 26.

---

    Code
      sparse_character(c("A", "B"), c(3, 1), 5)
    Condition
      Error in `sparse_character()`:
      ! `positions` must be sorted in increasing order.

---

    Code
      sparse_character("A", 10, 5)
    Condition
      Error in `sparse_character()`:
      x `positions` value must not be larger than `length`.
      i Offending values at index: 1.

---

    Code
      sparse_character(rep("A", 50), seq(25, 74), 50)
    Condition
      Error in `sparse_character()`:
      x `positions` value must not be larger than `length`.
      i Offending values at index: 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, ..., 49, and 50.

---

    Code
      sparse_character("A", 0, 5)
    Condition
      Error in `sparse_character()`:
      x `positions` value must positive.
      i Non-positive values at index: 1.

---

    Code
      sparse_character(rep("A", 101), seq(-50, 50), 100)
    Condition
      Error in `sparse_character()`:
      x `positions` value must positive.
      i Non-positive values at index: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 50, and 51.

---

    Code
      sparse_character("", 1, 10)
    Condition
      Error in `sparse_character()`:
      x `values` value must not be equal to the default .
      i  values at index: 1.

---

    Code
      sparse_character(rep(c("A", ""), 5), 1:10, 50)
    Condition
      Error in `sparse_character()`:
      x `values` value must not be equal to the default .
      i  values at index: 2, 4, 6, 8, and 10.

# default argument is working

    Code
      sparse_character("A", 1, 10, default = letters)
    Condition
      Error in `sparse_character()`:
      ! `default` must be a single string, not a character vector.

---

    Code
      sparse_character("A", 1, 10, default = TRUE)
    Condition
      Error in `sparse_character()`:
      ! `default` must be a single string, not `TRUE`.

---

    Code
      sparse_character(c("A", "B", "C"), c(1, 4, 6), 10, default = "A")
    Condition
      Error in `sparse_character()`:
      x `values` value must not be equal to the default A.
      i A values at index: 1.

# verbose testing

    Code
      sparse_character("A", 1, 1)[]
    Output
      sparsevctrs: Sparse vector materialized
      [1] "A"

# printing works #48

    Code
      sparse_character("A", 1, 10)[]
    Output
       [1] "A" ""  ""  ""  ""  ""  ""  ""  ""  "" 

