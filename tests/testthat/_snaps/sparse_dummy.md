# sparse_dummy() errors with wrong input

    Code
      sparse_dummy(letters)
    Condition
      Error in `sparse_dummy()`:
      ! `x` must be a factor, not a character vector.

---

    Code
      sparse_dummy(mtcars)
    Condition
      Error in `sparse_dummy()`:
      ! `x` must be a factor, not a data frame.

---

    Code
      sparse_dummy(1:5)
    Condition
      Error in `sparse_dummy()`:
      ! `x` must be a factor, not an integer vector.

---

    Code
      sparse_dummy(NULL)
    Condition
      Error in `sparse_dummy()`:
      ! `x` must be a factor, not NULL.

