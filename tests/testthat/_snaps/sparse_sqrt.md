# sparse_sqrt() errors for negative values

    Code
      sparse_sqrt(x)
    Condition
      Warning in `sqrt()`:
      NaNs produced
      Error in `sparse_double()`:
      x `values` must not contain NaN values.
      i NaN values at index: 1.

---

    Code
      sparse_sqrt(x)
    Condition
      Warning in `sqrt()`:
      NaNs produced
      Error in `sparse_double()`:
      ! `default` must be a number, not `NaN`.

