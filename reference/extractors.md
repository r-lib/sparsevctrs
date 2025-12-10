# Information extraction from sparse vectors

Extract positions, values, and default from sparse vectors without the
need to materialize vector.

## Usage

``` r
sparse_positions(x)

sparse_values(x)

sparse_default(x)
```

## Arguments

- x:

  vector to be extracted from.

## Value

vectors of requested attributes

## Details

`sparse_default()` returns `NA` when applied to non-sparse vectors. This
is done to have an indicator of non-sparsity.

for ease of use, these functions also works on non-sparse variables.

## Examples

``` r
x_sparse <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)
x_dense <- c(0, pi, 0, 0, 0.5, 0, 0, 0, 0, 0.1)

sparse_positions(x_sparse)
#> [1]  2  5 10
sparse_values(x_sparse)
#> [1] 3.141593 5.000000 0.100000
sparse_default(x_sparse)
#> [1] 0

sparse_positions(x_dense)
#>  [1]  1  2  3  4  5  6  7  8  9 10
sparse_values(x_dense)
#>  [1] 0.000000 3.141593 0.000000 0.000000 0.500000 0.000000 0.000000
#>  [8] 0.000000 0.000000 0.100000
sparse_default(x_dense)
#> [1] NA

x_sparse_3 <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10, default = 3)
sparse_default(x_sparse_3)
#> [1] 3
```
