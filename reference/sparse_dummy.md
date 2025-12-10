# Generate sparse dummy variables

Generate sparse dummy variables

## Usage

``` r
sparse_dummy(x, one_hot = TRUE)
```

## Arguments

- x:

  A factor.

- one_hot:

  A single logical value. Should the first factor level be included or
  not. Defaults to `FALSE`.

## Value

A list of sparse integer dummy variables.

## Details

Only factor variables can be used with `sparse_dummy()`. A call to
[`as.factor()`](https://rdrr.io/r/base/factor.html) would be required
for any other type of data.

If only a single level is present after `one_hot` takes effect. Then the
vector produced won't be sparse.

A missing value at the `i`th element will produce missing values for all
dummy variables at thr `i`th position.

## Examples

``` r
x <- factor(c("a", "a", "b", "c", "d", "b"))

sparse_dummy(x, one_hot = FALSE)
#> $b
#> [1] 0 0 1 0 0 1
#> 
#> $c
#> [1] 0 0 0 1 0 0
#> 
#> $d
#> [1] 0 0 0 0 1 0
#> 

x <- factor(c("a", "a", "b", "c", "d", "b"))

sparse_dummy(x, one_hot = TRUE)
#> $a
#> [1] 1 1 0 0 0 0
#> 
#> $b
#> [1] 0 0 1 0 0 1
#> 
#> $c
#> [1] 0 0 0 1 0 0
#> 
#> $d
#> [1] 0 0 0 0 1 0
#> 

x <- factor(c("a", NA, "b", "c", "d", NA))

sparse_dummy(x, one_hot = FALSE)
#> $b
#> [1]  0 NA  1  0  0 NA
#> 
#> $c
#> [1]  0 NA  0  1  0 NA
#> 
#> $d
#> [1]  0 NA  0  0  1 NA
#> 

x <- factor(c("a", NA, "b", "c", "d", NA))

sparse_dummy(x, one_hot = TRUE)
#> $a
#> [1]  1 NA  0  0  0 NA
#> 
#> $b
#> [1]  0 NA  1  0  0 NA
#> 
#> $c
#> [1]  0 NA  0  1  0 NA
#> 
#> $d
#> [1]  0 NA  0  0  1 NA
#> 
```
