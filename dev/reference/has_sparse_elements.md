# Check for sparse elements

This function checks to see if a data.frame, tibble or list contains one
or more sparse vectors.

## Usage

``` r
has_sparse_elements(x)
```

## Arguments

- x:

  a data frame, tibble, or list.

## Value

A single logical value.

## Details

The checking in this function is done using
[`is_sparse_vector()`](https://r-lib.github.io/sparsevctrs/dev/reference/type-predicates.md),
but is implemented using an early exit pattern to provide fast
performance for wide data.frames.

This function does not test whether `x` is a data.frame, tibble or list.
It simply iterates over the elements and sees if they are sparse
vectors.

## Examples

``` r
set.seed(1234)
n_cols <- 10000
mat <- matrix(sample(0:1, n_cols * 10, TRUE, c(0.9, 0.1)), ncol = n_cols)
colnames(mat) <- as.character(seq_len(n_cols))
sparse_mat <- Matrix::Matrix(mat, sparse = TRUE)

res <- coerce_to_sparse_tibble(sparse_mat)
has_sparse_elements(res)
#> [1] TRUE

has_sparse_elements(mtcars)
#> [1] FALSE
```
