# Create sparse double vector

Construction of vectors where only values and positions are recorded.
The Length and default values determine all other information.

## Usage

``` r
sparse_double(values, positions, length, default = 0)
```

## Arguments

- values:

  double vector, values of non-zero entries.

- positions:

  integer vector, indices of non-zero entries.

- length:

  integer value, Length of vector.

- default:

  double value, value at indices not specified by `positions`. Defaults
  to `0`. Cannot be `NA`.

## Value

sparse double vector

## Details

`values` and `positions` are expected to be the same length, and are
allowed to both have zero length.

Allowed values for `value` is double and integer values. integer values
will be coerced to doubles. Missing values such as `NA` and `NA_real_`
are allowed. Everything else is disallowed, This includes `Inf` and
`NaN`. The values are also not allowed to take the same value as
`default`.

`positions` should be integers or integer-like doubles. Everything else
is not allowed. Positions should furthermore be positive (`0` not
allowed), unique, and in increasing order. Lastly they should all be
smaller that `length`.

For developers:

setting `options("sparsevctrs.verbose_materialize" = TRUE)` will print a
message each time a sparse vector has been forced to materialize.

## See also

[`sparse_integer()`](https://r-lib.github.io/sparsevctrs/reference/sparse_integer.md)
[`sparse_character()`](https://r-lib.github.io/sparsevctrs/reference/sparse_character.md)

## Examples

``` r
sparse_double(numeric(), integer(), 10)
#>  [1] 0 0 0 0 0 0 0 0 0 0

sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)
#>  [1] 0.000000 3.141593 0.000000 0.000000 5.000000 0.000000 0.000000
#>  [8] 0.000000 0.000000 0.100000

str(
  sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 1000000000)
)
#>  num [1:1000000000] 0 3.14 0 0 5 ...
```
