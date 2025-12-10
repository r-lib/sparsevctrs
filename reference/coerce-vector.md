# Coerce numeric vector to sparse double

Takes a numeric vector, integer or double, and turn it into a sparse
double vector.

## Usage

``` r
as_sparse_double(x, default = 0)

as_sparse_integer(x, default = 0L)

as_sparse_character(x, default = "")

as_sparse_logical(x, default = FALSE)
```

## Arguments

- x:

  a numeric vector.

- default:

  default value to use. Defaults to `0`.

  The values of `x` must be double or integer. It must not contain any
  `Inf` or `NaN` values.

## Value

sparse vectors

## Examples

``` r
x_dense <- c(3, 0, 2, 0, 0, 0, 4, 0, 0, 0)
x_sparse <- as_sparse_double(x_dense)
x_sparse
#>  [1] 3 0 2 0 0 0 4 0 0 0

is_sparse_double(x_sparse)
#> [1] TRUE
```
