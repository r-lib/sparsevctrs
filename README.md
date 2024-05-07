
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparsevctrs

<!-- badges: start -->

[![R-CMD-check](https://github.com/EmilHvitfeldt/sparsevctrs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EmilHvitfeldt/sparsevctrs/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/EmilHvitfeldt/sparsevctrs/branch/main/graph/badge.svg)](https://app.codecov.io/gh/EmilHvitfeldt/sparsevctrs?branch=main)
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

x <- sparse_double(4, 7, 10)

x
#>  [1] 0 0 0 0 0 0 4 0 0 0
sum(x)
#> [1] 4

sparse_double(4, 7, 10) + sparse_double(3, 2, 10)
#>  [1] 3.456532e-314 2.847320e-314 2.714492e-314 2.927164e-314 4.940656e-323
#>  [6]  0.000000e+00  0.000000e+00  3.000000e+00  0.000000e+00  0.000000e+00
```

This class is compatible with tibbles

``` r
library(tibble)

tibble(x = sample(1:10), y = sparse_double(1, 7, 10))
#> # A tibble: 10 × 2
#>        x     y
#>    <int> <dbl>
#>  1    10     0
#>  2     6     0
#>  3     5     0
#>  4     4     0
#>  5     1     0
#>  6     8     0
#>  7     2     1
#>  8     7     0
#>  9     9     0
#> 10     3     0
```
