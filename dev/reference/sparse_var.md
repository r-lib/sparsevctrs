# Calculate variance from sparse vectors

Calculate variance from sparse vectors

## Usage

``` r
sparse_var(x, na_rm = FALSE)
```

## Arguments

- x:

  A sparse numeric vector.

- na_rm:

  Logical, whether to remove missing values. Defaults to `FALSE`.

## Value

single numeric value.

## Details

This function, as with any of the other helper functions assumes that
the input `x` is a sparse numeric vector. This is done for performance
reasons, and it is thus the users responsibility to perform input
checking.

Much like [`var()`](https://rdrr.io/r/stats/cor.html) it uses the
denominator `n-1`.

## Examples

``` r
sparse_var(
  sparse_double(1000, 1, 1000)
)
#> [1] 1000

sparse_var(
  sparse_double(1000, 1, 1000, default = 1)
)
#> [1] 998.001

sparse_var(
  sparse_double(c(10, 50, 11), c(1, 50, 111), 1000)
)
#> [1] 2.718678

sparse_var(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
)
#> [1] NA

sparse_var(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
  na_rm = TRUE
)
#> [1] 0.2210006
```
