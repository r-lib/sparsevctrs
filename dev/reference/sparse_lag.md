# Compute lagged values for sparse vectors

Compute lagged values for sparse vectors

## Usage

``` r
sparse_lag(x, n = 1L, default = NULL)
```

## Arguments

- x:

  A sparse vector.

- n:

  Positive integer of length 1, giving the number of positions to lag
  by.

- default:

  The value used to pad
  ``` x`` back to its original size after the lag has been applied. The default,  ```NULL“,
  pads with a missing value.

## Value

sparse vector.

## Details

This function, as with any of the other helper functions assumes that
the input `x` is a sparse numeric vector. This is done for performance
reasons, and it is thus the users responsibility to perform input
checking.

## Examples

``` r
vec_dbl <- sparse_double(c(pi, 4, 5/2), c(1, 5, 7), 10)

sparse_lag(vec_dbl)
#>  [1]       NA 3.141593 0.000000 0.000000 0.000000 4.000000 0.000000
#>  [8] 2.500000 0.000000 0.000000
sparse_lag(vec_dbl, n = 3)
#>  [1]       NA       NA       NA 3.141593 0.000000 0.000000 0.000000
#>  [8] 4.000000 0.000000 2.500000
sparse_lag(vec_dbl, n = 3, default = 0)
#>  [1] 0.000000 0.000000 0.000000 3.141593 0.000000 0.000000 0.000000
#>  [8] 4.000000 0.000000 2.500000

vec_int <- sparse_integer(c(1, 2, 3), c(1, 5, 7), 10)

sparse_lag(vec_int)
#>  [1] NA  1  0  0  0  2  0  3  0  0
sparse_lag(vec_int, n = 3)
#>  [1] NA NA NA  1  0  0  0  2  0  3
sparse_lag(vec_int, n = 3, default = 0L)
#>  [1] 0 0 0 1 0 0 0 2 0 3

vec_chr <- sparse_character(c("A", "B", "C"), c(1, 5, 7), 10)

sparse_lag(vec_chr)
#>  [1] NA  "A" ""  ""  ""  "B" ""  "C" ""  "" 
sparse_lag(vec_chr, n = 3)
#>  [1] NA  NA  NA  "A" ""  ""  ""  "B" ""  "C"
sparse_lag(vec_chr, n = 3, default = "before")
#>  [1] "before" "before" "before" "A"      ""       ""       ""      
#>  [8] "B"      ""       "C"     

vec_lgl <- sparse_logical(c(TRUE, TRUE, TRUE), c(1, 5, 7), 10)

sparse_lag(vec_lgl)
#>  [1]    NA  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE
sparse_lag(vec_lgl, n = 3)
#>  [1]    NA    NA    NA  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE
sparse_lag(vec_lgl, n = 3, default = FALSE)
#>  [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE
```
