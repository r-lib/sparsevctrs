# Sparse vector type checkers

Helper functions to determine whether an vector is a sparse vector or
not.

## Usage

``` r
is_sparse_vector(x)

is_sparse_numeric(x)

is_sparse_double(x)

is_sparse_integer(x)

is_sparse_character(x)

is_sparse_logical(x)
```

## Arguments

- x:

  value to be checked.

## Value

single logical value

## Details

`is_sparse_vector()` is a general function that detects any type of
sparse vector created with this package. `is_sparse_double()`,
`is_sparse_integer()`, `is_sparse_character()`, and
`is_sparse_logical()` are more specific functions that only detects the
type. `is_sparse_numeric()` matches both sparse integers and doubles.

## Examples

``` r
x_sparse <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)
x_dense <- c(0, pi, 0, 0, 0.5, 0, 0, 0, 0, 0.1)

is_sparse_vector(x_sparse)
#> [1] TRUE
is_sparse_vector(x_dense)
#> [1] FALSE

is_sparse_double(x_sparse)
#> [1] TRUE
is_sparse_double(x_dense)
#> [1] FALSE

is_sparse_character(x_sparse)
#> [1] FALSE
is_sparse_character(x_dense)
#> [1] FALSE

# Forced materialization
is_sparse_vector(x_sparse[])
#> [1] FALSE
```
