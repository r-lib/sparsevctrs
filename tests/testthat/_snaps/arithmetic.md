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
      sparse_division_scalar(vec_lgl, 2)
    Condition
      Error in `sparse_division_scalar()`:
      ! `x` must be a sparse integer or double vector.

---

    Code
      sparse_division_scalar(vec_chr, 2)
    Condition
      Error in `sparse_division_scalar()`:
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

# scalar arithmetic errors for non-numeric val

    Code
      sparse_addition_scalar(vec_dbl, "a")
    Condition
      Error in `sparse_addition_scalar()`:
      ! `val` must be a single number.

# scalar arithmetic on sparse integers needs whole number val

    Code
      sparse_multiplication_scalar(vec_int, 0.3)
    Condition
      Error in `sparse_multiplication_scalar()`:
      ! `val` must be a whole number when `x` is a sparse integer vector.

---

    Code
      sparse_addition_scalar(vec_int, 0.5)
    Condition
      Error in `sparse_addition_scalar()`:
      ! `val` must be a whole number when `x` is a sparse integer vector.

---

    Code
      sparse_multiplication_scalar(vec_int, 0.5)
    Condition
      Error in `sparse_multiplication_scalar()`:
      ! `val` must be a whole number when `x` is a sparse integer vector.

---

    Code
      sparse_multiplication_scalar(vec_odd, 0.5)
    Condition
      Error in `sparse_multiplication_scalar()`:
      ! `val` must be a whole number when `x` is a sparse integer vector.

---

    Code
      sparse_subtraction_scalar(vec_int, Inf)
    Condition
      Error in `sparse_subtraction_scalar()`:
      ! `val` must be a whole number when `x` is a sparse integer vector.

# scalar arithmetic on sparse integers handles overflow

    Code
      sparse_addition_scalar(vec_default, 1L)
    Condition
      Error in `sparse_addition_scalar()`:
      ! Integer overflow in the default value of `x`.

# scalar arithmetic errors for results that can't be stored

    Code
      sparse_multiplication_scalar(vec_dbl, Inf)
    Condition
      Error in `sparse_multiplication_scalar()`:
      ! The result has a `NaN` default value, which sparse double vectors can't store.

---

    Code
      sparse_addition_scalar(vec_dbl, Inf)
    Condition
      Error in `sparse_addition_scalar()`:
      ! The result has `Inf` or `NaN` values, which sparse double vectors can't store.

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

