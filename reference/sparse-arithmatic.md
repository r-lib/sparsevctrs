# Vector arithmatic with sparse vectors

Do arithmatic operations on sparse vectors while trying to void
destroying the sparsity.

## Usage

``` r
sparse_multiplication(x, y)
```

## Arguments

- x:

  A numeric vector.

- y:

  A numeric vector.

## Value

A sparse vector of same type.

## Details

Note that this function works with both sparse and dense vectors for
both `x` and `y`, returning a sparse or dense vector according to the
input.

For `sparse_multiplication()` the class of the resulting vector depends
on the classes of `x` and `y`. If both `x` and `y` are integer vectors
then an integer vector is returned, otherwise a double vector is
returned.

`sparse_multiplication()` will return a non-sparse vector if both `x`
and `y` is non-sparse. Otherwise a sparse vector is returned.

`sparse_multiplication()` will destroy sparsity of sparse vectors with
non-0 `default` values.

## Examples

``` r
x_sparse <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)

sparse_multiplication(x_sparse, x_sparse)
#>  [1]  0.000000  9.869604  0.000000  0.000000 25.000000  0.000000
#>  [7]  0.000000  0.000000  0.000000  0.010000
```
