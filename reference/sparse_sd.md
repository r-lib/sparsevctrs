# Calculate standard diviation from sparse vectors

Calculate standard diviation from sparse vectors

## Usage

``` r
sparse_sd(x, na_rm = FALSE)
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

Much like [`sd()`](https://rdrr.io/r/stats/sd.html) it uses the
denominator `n-1`.

## Examples

``` r
sparse_sd(
  sparse_double(1000, 1, 1000)
)
#> [1] 31.62278

sparse_sd(
  sparse_double(1000, 1, 1000, default = 1)
)
#> [1] 31.59115

sparse_sd(
  sparse_double(c(10, 50, 11), c(1, 50, 111), 1000)
)
#> [1] 1.648841

sparse_sd(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
)
#> [1] NA

sparse_sd(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
  na_rm = TRUE
)
#> [1] 0.470107
```
