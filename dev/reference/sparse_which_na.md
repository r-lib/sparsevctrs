# Which indices are Missing Values

Which indices are Missing Values

## Usage

``` r
sparse_which_na(x)
```

## Arguments

- x:

  A sparse vector.

## Value

A logical vector.

## Details

This function, as with any of the other helper functions assumes that
the input `x` is a sparse numeric vector. This is done for performance
reasons, and it is thus the users responsibility to perform input
checking.

## See also

[`sparse_is_na()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_is_na.md)

## Examples

``` r
sparse_which_na(
  sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
)
#> [1] 50
```
