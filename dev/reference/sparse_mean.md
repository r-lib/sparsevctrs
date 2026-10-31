# Calculate mean from sparse vectors

Calculate mean from sparse vectors

## Usage

``` r
sparse_mean(x, wts = NULL, na_rm = FALSE)
```

## Arguments

- x:

  A sparse numeric vector.

- wts:

  A numeric vector, should be same length as `x`.

- na_rm:

  Logical, whether to remove missing values. Defaults to `FALSE`.

## Value

single numeric value.

## Details

This function, as with any of the other helper functions assumes that
the input `x` is a sparse numeric vector. This is done for performance
reasons, and it is thus the users responsibility to perform input
checking.

When `wts` is supplied, the weights are multiplied into `x` and the
result is divided by the sum of the weights. With `na_rm = TRUE`,
weights at positions where the weighted value is missing are dropped
from the sum of the weights, so a missing weight removes that
observation instead of returning `NA` as
[`stats::weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html)
does.

## Examples

``` r
sparse_mean(
  sparse_double(1000, 1, 1000)
)
#> [1] 1

sparse_mean(
  sparse_double(1000, 1, 1000, default = 1)
)
#> [1] 1.999

sparse_mean(
  sparse_double(c(10, 50, 11), c(1, 50, 111), 1000)
)
#> [1] 0.071

sparse_mean(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
)
#> [1] NA

sparse_mean(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
  na_rm = TRUE
)
#> [1] 0.02102102
```
