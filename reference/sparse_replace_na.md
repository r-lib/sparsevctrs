# Replace NAs with specified values in sparse vectors

Replace NAs with specified values in sparse vectors

## Usage

``` r
sparse_replace_na(x, replace)
```

## Arguments

- x:

  A sparse vector.

- replace:

  A single value.

## Value

A sparse vector.

## Details

This function, as with any of the other helper functions assumes that
the input `x` is a sparse numeric vector. This is done for performance
reasons, and it is thus the users responsibility to perform input
checking. The `replace` is likewise not type or length checked.

The output type will match the values after coercion happens during
replacement.

## Examples

``` r
sparse_replace_na(
  sparse_double(c(10, NA, 11), c(1, 5, 10), 10),
  5
)
#>  [1] 10  0  0  0  5  0  0  0  0 11

sparse_replace_na(
  sparse_integer(c(10L, NA, 11L), c(1, 5, 10), 10),
  5L
)
#>  [1] 10  0  0  0  5  0  0  0  0 11

sparse_replace_na(
  sparse_character(c("A", NA, "E"), c(2, 5, 10), 10),
  "missing"
)
#>  [1] ""        "A"       ""        ""        "missing" ""       
#>  [7] ""        ""        ""        "E"      
```
