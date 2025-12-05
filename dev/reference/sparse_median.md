# Calculate median from sparse vectors

Calculate median from sparse vectors

## Usage

``` r
sparse_median(x, na_rm = FALSE)
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

## Examples

``` r
sparse_median(
  sparse_double(1000, 1, 1000)
)
#> [1] 0

sparse_median(
  sparse_double(1000, 1, 1000, default = 1)
)
#> [1] 1

sparse_median(
  sparse_double(c(10, 50, 11), c(1, 50, 111), 1000)
)
#> [1] 0

sparse_median(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
)
#> [1] NA

sparse_median(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
  na_rm = TRUE
)
#> [1] 0
```
