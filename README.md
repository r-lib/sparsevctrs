
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sparsevctrs <a href="https://r-lib.github.io/sparsevctrs/"><img src="man/figures/logo.png" align="right" height="138" alt="sparsevctrs website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-lib/sparsevctrs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/sparsevctrs/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/sparsevctrs/graph/badge.svg)](https://app.codecov.io/gh/r-lib/sparsevctrs)
<!-- badges: end -->

The goal of sparsevctrs is to provide a sparse vector
[ALTREP](https://svn.r-project.org/R/branches/ALTREP/ALTREP.html) class.
With this, you can have sparse data in the form of sparse columns in
`data.frame` or [tibble](https://tibble.tidyverse.org/). Due to the
nature of how ALTREP vectors work, these sparse vectors will behave like
the normal dense vectors you are used you. The vectors will contain
their sparseness as much as they can, and only materialize when they
have to.

## Installation

You can install the development version of sparsevctrs like so:

``` r
remotes::install_github("r-lib/sparsevctrs")
```

## Examples

A sparse vector, here specifically a sparse double vector, will be
identical to its dense counterpart, often with a smaller memory
footprint.

``` r
library(sparsevctrs)
library(lobstr)

x_sparse <- sparse_double(value = c(3, 1, 10), position = c(2, 7, 15), length = 1000)
x_dense <- numeric(1000)
x_dense[2] <- 3
x_dense[7] <- 1
x_dense[15] <- 10

obj_size(x_sparse)
#> 936 B
obj_size(x_dense)
#> 8.05 kB

identical(x_sparse, x_dense)
#> [1] TRUE
```

The memory of a sparse vector is proportional to the number of elements
plus a constant. This means that increasing the length of a sparse
vector doesn’t increase how much memory it uses. Unlike dense vectors
who has a much smaller constant, but increases according to the length
of the values.

``` r
x_sparse_0 <- sparse_double(numeric(), integer(), length = 0)
x_sparse_1000 <- sparse_double(numeric(), integer(), length = 1000)
x_sparse_1000000 <- sparse_double(numeric(), integer(), length = 10000000)

obj_size(x_sparse_0)
#> 888 B
obj_size(x_sparse_1000)
#> 888 B
obj_size(x_sparse_1000000)
#> 888 B

x_dense_0 <- numeric(0)
x_dense_1000 <- numeric(1000)
x_dense_1000000 <- numeric(10000000)

obj_size(x_dense_0)
#> 48 B
obj_size(x_dense_1000)
#> 8.05 kB
obj_size(x_dense_1000000)
#> 80.00 MB
```

These sparse vectors are compatible with tibbles and data frames.

``` r
library(tibble)
set.seed(1234)

tibble(
  x = sample(1:1000),
  y = sparse_double(1, 7, 1000)
)
#> # A tibble: 1,000 × 2
#>        x     y
#>    <int> <dbl>
#>  1   284     0
#>  2   848     0
#>  3   918     0
#>  4   101     0
#>  5   623     0
#>  6   905     0
#>  7   645     1
#>  8   934     0
#>  9   400     0
#> 10   900     0
#> # ℹ 990 more rows
```

## Motivation

Sparse data happens from ingestion and preprocessing calculations. text
to counts, dummy variables etc etc

There are computational tools for calculations using sparse matrices,
specifically the Matrix package and some modeling packages (e.g.,
xgboost, glmnet, etc.). We want to utilize these tools as best we can
without making redundant implementations.

However, sparse matrices are not great for data in general, or at least
not until the very end, when mathematical calculations occur. Converting
everything to “numeric” is problematic for dates, factors, etc. There
are good reasons why data frames were created in the first place.
Matrices are efficient but primitive.

The problem is that many tools, especially the tidyverse, rely on data
frames since they are more expressive and accommodate different variable
types. We need to merge and filter rows/columns, etc, in a flexible and
user-friendly way. (joins, pivoting)

Having a sparse representation of data that allows us to use modern data
manipulation interfaces, keeps memory overhead low, and can be
efficiently converted to a more primitive matrix format so that we can
let Matrix and other packages do what they do best.

This is achieved with this package, by providing sparse vectors that fit
into a data frame. Along with converting tools between sparse matrices
and data frames.
