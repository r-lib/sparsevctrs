# scalar arithmetic errors for non-numeric sparse vectors

    Code
      sparse_multiplication_scalar(vec_lgl, 2)
    Condition
      Error in `sparse_multiplication_scalar()`:
      ! `x` must be a sparse integer or double vector.

---

    Code
      sparse_multiplication_scalar(vec_chr, 2)
    Condition
      Error in `sparse_multiplication_scalar()`:
      ! `x` must be a sparse integer or double vector.

---

    Code
      sparse_addition_scalar(vec_lgl, 2)
    Condition
      Error in `sparse_addition_scalar()`:
      ! `x` must be a sparse integer or double vector.

---

    Code
      sparse_addition_scalar(vec_chr, 2)
    Condition
      Error in `sparse_addition_scalar()`:
      ! `x` must be a sparse integer or double vector.

---

    Code
      sparse_subtraction_scalar(vec_lgl, 2)
    Condition
      Error in `sparse_subtraction_scalar()`:
      ! `x` must be a sparse integer or double vector.

---

    Code
      sparse_subtraction_scalar(vec_chr, 2)
    Condition
      Error in `sparse_subtraction_scalar()`:
      ! `x` must be a sparse integer or double vector.

# vector multiplication errors for non-numeric input

    Code
      sparse_multiplication("a", 1)
    Condition
      Error in `sparse_multiplication()`:
      ! `x` must be numeric, not a string.

---

    Code
      sparse_multiplication(1, "a")
    Condition
      Error in `sparse_multiplication()`:
      ! `y` must be numeric, not a string.

