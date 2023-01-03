
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparsevctrs

<!-- badges: start -->
<!-- badges: end -->

THIS IS A PROTOTYPE. DO NOT USE

The goal of sparsevctrs is to provide a sparse vector class that is
compatible with tibbles.

## Installation

You can install the development version of sparsevctrs like so:

``` r
remotes::install_github("emilhvitfeldt/sparsevctrs")
```

## Example

``` r
library(sparsevctrs)

x <- new_sparse_vector(4, 7, 10)

x
#> <sparse_vector[10]>
#>  [1] 0 0 0 0 0 0 4 0 0 0
sum(x)
#> [1] 4

new_sparse_vector(4, 7, 10) + new_sparse_vector(3, 2, 10)
#> <sparse_vector[10]>
#>  [1] 0 3 0 0 0 0 4 0 0 0
```

This class is compatible with tibbles

``` r
library(tibble)

tibble(x = sample(1:10), y = new_sparse_vector(1, 7, 10))
#> # A tibble: 10 × 2
#>        x       y
#>    <int> <spvtr>
#>  1    10       0
#>  2     6       0
#>  3     5       0
#>  4     4       0
#>  5     1       0
#>  6     8       0
#>  7     2       1
#>  8     7       0
#>  9     9       0
#> 10     3       0
```
