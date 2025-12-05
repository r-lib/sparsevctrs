# Create sparse integer vector

Construction of vectors where only values and positions are recorded.
The Length and default values determine all other information.

## Usage

``` r
sparse_integer(values, positions, length, default = 0L)
```

## Arguments

- values:

  integer vector, values of non-zero entries.

- positions:

  integer vector, indices of non-zero entries.

- length:

  integer value, Length of vector.

- default:

  integer value, value at indices not specified by `positions`. Defaults
  to `0L`. Cannot be `NA`.

## Value

sparse integer vector

## Details

`values` and `positions` are expected to be the same length, and are
allowed to both have zero length.

Allowed values for `value` is integer values. This means that the double
vector `c(1, 5, 4)` is accepted as it can be losslessly converted to the
integer vector `c(1L, 5L, 4L)`. Missing values such as `NA` and
`NA_real_` are allowed. Everything else is disallowed, This includes
`Inf` and `NaN`. The values are also not allowed to take the same value
as `default`.

`positions` should be integers or integer-like doubles. Everything else
is not allowed. Positions should furthermore be positive (`0` not
allowed), unique, and in increasing order. Lastly they should all be
smaller that `length`.

For developers:

setting `options("sparsevctrs.verbose_materialize" = TRUE)` will print a
message each time a sparse vector has been forced to materialize.

## See also

[`sparse_double()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_double.md)
[`sparse_character()`](https://r-lib.github.io/sparsevctrs/dev/reference/sparse_character.md)

## Examples

``` r
sparse_integer(integer(), integer(), 10)
#>  [1] 0 0 0 0 0 0 0 0 0 0

sparse_integer(c(4, 5, 7), c(2, 5, 10), 10)
#>  [1] 0 4 0 0 5 0 0 0 0 7

str(
  sparse_integer(c(4, 5, 7), c(2, 5, 10), 1000000000)
)
#>  int [1:1000000000] 0 4 0 0 5 0 0 0 0 7 ...
```
