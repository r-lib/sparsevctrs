# Scalar arithmetic with sparse vectors

Do Arithmetic on sparse vectors without destroying the sparsity. Note
that only multiplication and division preserves the default value.

## Usage

``` r
sparse_division_scalar(x, val)

sparse_multiplication_scalar(x, val)

sparse_addition_scalar(x, val)

sparse_subtraction_scalar(x, val)
```

## Arguments

- x:

  A sparse vector.

- val:

  A single numeric value.

## Value

A sparse vector of same type.

## Details

Only minimal checking of the inputs is done, for performance reasons.

`sparse_division_scalar()` and `sparse_multiplication_scalar()` are the
most used ones, as they preserve the default, which is often what you
want to do.

`sparse_division_scalar()` always produces double vectors, regardless of
whether they could be represented as integers or not. Expect when
`val = 1` as the input is returned unchanged, or `val = NA` as the input
returned will be `NA` or the appropiate type. When `val = 0` the result
contains `Inf`, `-Inf` and `NaN`, which sparse vectors can't store, so a
regular (dense) double vector is returned.

The other functions return a sparse vector of the same type as `x`. This
means that when `x` is a sparse integer vector, `val` must give whole
number results. Integer overflow in the stored values produces `NA` with
a warning, as it does for dense integer vectors.

Results that sparse double vectors can't store, such as stored values
that are `Inf` or `NaN`, or a default value that is `NaN`, give an
error.

## Examples

``` r
x_sparse <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)

sparse_division_scalar(x_sparse, 2)
#>  [1] 0.000000 1.570796 0.000000 0.000000 2.500000 0.000000 0.000000
#>  [8] 0.000000 0.000000 0.050000
sparse_multiplication_scalar(x_sparse, 2)
#>  [1]  0.000000  6.283185  0.000000  0.000000 10.000000  0.000000
#>  [7]  0.000000  0.000000  0.000000  0.200000
sparse_addition_scalar(x_sparse, 2)
#>  [1] 2.000000 5.141593 2.000000 2.000000 7.000000 2.000000 2.000000
#>  [8] 2.000000 2.000000 2.100000
sparse_subtraction_scalar(x_sparse, 2)
#>  [1] -2.000000  1.141593 -2.000000 -2.000000  3.000000 -2.000000
#>  [7] -2.000000 -2.000000 -2.000000 -1.900000
```
