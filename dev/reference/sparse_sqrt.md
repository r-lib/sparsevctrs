# Calculate sqrt of sparse vectors

Calculate sqrt of sparse vectors

## Usage

``` r
sparse_sqrt(x)
```

## Arguments

- x:

  A sparse numeric vector.

## Value

A sparse double vector.

## Details

This function, as with any of the other helper functions assumes that
the input `x` is a sparse numeric vector. This is done for performance
reasons, and it is thus the users responsibility to perform input
checking.

The output will be a double vector regardless of the input type.

## Examples

``` r
sparse_sqrt(
  sparse_double(1000, 1, 10)
)
#>  [1] 31.62278  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
#>  [8]  0.00000  0.00000  0.00000

sparse_sqrt(
  sparse_integer(1000, 3, 10, default = 2)
)
#>  [1]  1.414214  1.414214 31.622777  1.414214  1.414214  1.414214
#>  [7]  1.414214  1.414214  1.414214  1.414214

sparse_sqrt(
  sparse_double(c(10, NA, 11), c(1, 5, 10), 10)
)
#>  [1] 3.162278 0.000000 0.000000 0.000000       NA 0.000000 0.000000
#>  [8] 0.000000 0.000000 3.316625
```
