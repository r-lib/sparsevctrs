# Coerce sparse data frame to sparse matrix

Turning data frame with sparse columns into sparse matrix using
[`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html).

## Usage

``` r
coerce_to_sparse_matrix(x, call = rlang::caller_env(0))
```

## Arguments

- x:

  a data frame or tibble with sparse columns.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

sparse matrix

## Details

No checking is currently do to `x` to determine whether it contains
sparse columns or not. Thus it works with any data frame. Needless to
say, creating a sparse matrix out of a dense data frame is not ideal.

## See also

[`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_data_frame.md)
[`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_tibble.md)

## Examples

``` r
sparse_tbl <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
names(sparse_tbl) <- letters[1:10]
sparse_tbl <- as.data.frame(sparse_tbl)
sparse_tbl
#>    a b c d e f g h i  j
#> 1  1 0 0 0 0 0 0 0 0  0
#> 2  0 2 0 0 0 0 0 0 0  0
#> 3  0 0 3 0 0 0 0 0 0  0
#> 4  0 0 0 4 0 0 0 0 0  0
#> 5  0 0 0 0 5 0 0 0 0  0
#> 6  0 0 0 0 0 6 0 0 0  0
#> 7  0 0 0 0 0 0 7 0 0  0
#> 8  0 0 0 0 0 0 0 8 0  0
#> 9  0 0 0 0 0 0 0 0 9  0
#> 10 0 0 0 0 0 0 0 0 0 10

res <- coerce_to_sparse_matrix(sparse_tbl)
res
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names ‘a’, ‘b’, ‘c’ ... ]]
#>                           
#>  [1,] 1 . . . . . . . .  .
#>  [2,] . 2 . . . . . . .  .
#>  [3,] . . 3 . . . . . .  .
#>  [4,] . . . 4 . . . . .  .
#>  [5,] . . . . 5 . . . .  .
#>  [6,] . . . . . 6 . . .  .
#>  [7,] . . . . . . 7 . .  .
#>  [8,] . . . . . . . 8 .  .
#>  [9,] . . . . . . . . 9  .
#> [10,] . . . . . . . . . 10
```
