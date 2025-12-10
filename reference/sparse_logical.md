# Create sparse logical vector

Construction of vectors where only values and positions are recorded.
The Length and default values determine all other information.

## Usage

``` r
sparse_logical(values, positions, length, default = FALSE)
```

## Arguments

- values:

  logical vector, values of non-zero entries.

- positions:

  integer vector, indices of non-zero entries.

- length:

  integer value, Length of vector.

- default:

  logical value, value at indices not specified by `positions`. Defaults
  to `FALSE`. Cannot be `NA`.

## Value

sparse logical vector

## Details

`values` and `positions` are expected to be the same length, and are
allowed to both have zero length.

Allowed values for `value` are logical values. Missing values such as
`NA` and `NA_real_` are allowed. Everything else is disallowed, The
values are also not allowed to take the same value as `default`.

`positions` should be integers or integer-like doubles. Everything else
is not allowed. Positions should furthermore be positive (`0` not
allowed), unique, and in increasing order. Lastly they should all be
smaller that `length`.

For developers:

setting `options("sparsevctrs.verbose_materialize" = TRUE)` will print a
message each time a sparse vector has been forced to materialize.

## See also

[`sparse_double()`](https://r-lib.github.io/sparsevctrs/reference/sparse_double.md)
[`sparse_integer()`](https://r-lib.github.io/sparsevctrs/reference/sparse_integer.md)
[`sparse_character()`](https://r-lib.github.io/sparsevctrs/reference/sparse_character.md)

## Examples

``` r
sparse_logical(logical(), integer(), 10)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

sparse_logical(c(TRUE, NA, TRUE), c(2, 5, 10), 10)
#>  [1] FALSE  TRUE FALSE FALSE    NA FALSE FALSE FALSE FALSE  TRUE

str(
  sparse_logical(c(TRUE, NA, TRUE), c(2, 5, 10), 1000000000)
)
#>  logi [1:1000000000] FALSE TRUE FALSE FALSE NA FALSE ...
```
