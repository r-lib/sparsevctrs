# Coerce sparse matrix to tibble with sparse columns

Turning a sparse matrix into a tibble.

## Usage

``` r
coerce_to_sparse_tibble(x, call = rlang::caller_env(0))
```

## Arguments

- x:

  sparse matrix.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

tibble with sparse columns

## Details

The only requirement from the sparse matrix is that it contains column
names.

## See also

[`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_data_frame.md)
[`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/dev/reference/coerce_to_sparse_matrix.md)

## Examples

``` r
set.seed(1234)
mat <- matrix(sample(0:1, 100, TRUE, c(0.9, 0.1)), nrow = 10)
colnames(mat) <- letters[1:10]
sparse_mat <- Matrix::Matrix(mat, sparse = TRUE)
sparse_mat
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names ‘a’, ‘b’, ‘c’ ... ]]
#>                          
#>  [1,] . . . . . . . . 1 .
#>  [2,] . . . . . . . . . 1
#>  [3,] . . . . . . . . . .
#>  [4,] . 1 . . . . . . . .
#>  [5,] . . . . . . . . . .
#>  [6,] . . . . . . . . . .
#>  [7,] . . . . . . . . . .
#>  [8,] . . 1 . . . . . . .
#>  [9,] . . . 1 . . . . . .
#> [10,] . . . . . . . . . .

res <- coerce_to_sparse_tibble(sparse_mat)
res
#> # A tibble: 10 × 10
#>        a     b     c     d     e     f     g     h     i     j
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     0     0     0     0     0     0     0     0     1     0
#>  2     0     0     0     0     0     0     0     0     0     1
#>  3     0     0     0     0     0     0     0     0     0     0
#>  4     0     1     0     0     0     0     0     0     0     0
#>  5     0     0     0     0     0     0     0     0     0     0
#>  6     0     0     0     0     0     0     0     0     0     0
#>  7     0     0     0     0     0     0     0     0     0     0
#>  8     0     0     1     0     0     0     0     0     0     0
#>  9     0     0     0     1     0     0     0     0     0     0
#> 10     0     0     0     0     0     0     0     0     0     0

# All columns are sparse
vapply(res, is_sparse_vector, logical(1))
#>    a    b    c    d    e    f    g    h    i    j 
#> TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE 
```
