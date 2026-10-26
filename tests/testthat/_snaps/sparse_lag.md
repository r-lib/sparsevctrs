# sparse_lag() errors for n below 1

    Code
      sparse_lag(vec, 0)
    Condition
      Error in `sparse_lag()`:
      ! `n` must be at least 1, not 0.

---

    Code
      sparse_lag(vec, -1)
    Condition
      Error in `sparse_lag()`:
      ! `n` must be at least 1, not -1.

