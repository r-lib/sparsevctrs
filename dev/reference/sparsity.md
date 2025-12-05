# Calculate sparsity of data frames, matrices, and sparse matrices

Turning data frame with sparse columns into sparse matrix using
[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html).

## Usage

``` r
sparsity(x, sample = NULL)
```

## Arguments

- x:

  a data frame, matrix of sparse matrix.

- sample:

  a integer or `NULL`. Number of rows to sample to estimate sparsity. If
  `NULL` then no sampling is performed. Will not be used when `x` is a
  sparse matrix. Defaults to `NULL`.

## Value

a single number, between 0 and 1.

## Details

Only numeric 0s are considered zeroes in this calculations. Missing
values, logical vectors and then string `"0"` aren't counted.

## Examples

``` r
# data frame
sparsity(mtcars)
#> [1] 0.1051136

# Matrix
set.seed(1234)
mat <- matrix(sample(0:1, 100, TRUE, c(0.9, 0.1)), nrow = 10)
colnames(mat) <- letters[1:10]

sparsity(mat)
#> [1] 0.95

# Sparse matrix
sparse_mat <- Matrix::Matrix(mat, sparse = TRUE)

sparsity(sparse_mat)
#> [1] 0.95
```
