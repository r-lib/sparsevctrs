# Design behind sparsevctrs

``` r
library(sparsevctrs)
```

The sparsevctrs package produces 3 things; ALTREP classes,
matrix/data.frame converting functions, helper functions. This document
outlines the rationale behind each of these and the decisions behind
them.

The primary objective of this package is to provide tools to work with
sparse data in data.frames/tibbles. The next highest priority is
execution speed. This means that algorithms and methods in this package
are written to minimize memory allocations whenever possible, once that
is done, running the code as fast as we can. These choices are made
because this package was written to deal with tasks that were otherwise
not possible due to memory constraints.

## Altrep Functions

The functions
[`sparse_double()`](https://r-lib.github.io/sparsevctrs/reference/sparse_double.md)
and its relatives are used to construct sparse vectors of the noted
type. To work they all need 4 pieces of information:

- `values`
- `positions`
- `length`
- `default` (defaults to 0)

The values need to match the type of the function name or be easily
coerced into the type (double -\> integer). The positions should be
integers or doubles that can losslessly be turned into integers. The
length should be a single non-negative integer-like value.

Values and positions are paired, and will thus be expected to be the
same length, furthermore, positions are expected to be sorted in
increasing order with no duplicates. The ordering is done to let the
various extraction methods work as efficiently as possible.

These functions have quite strict input checking by choice, to allow the
inner workings to be as efficient as possible.

The input of these functions mirrors the values stored in the ALTREP
class that they produce.

## Converting Functions

3 functions fall into this category:

- [`coerce_to_sparse_data_frame()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_data_frame.md)
- [`coerce_to_sparse_tibble()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_tibble.md)
- [`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_matrix.md)

the first two take a sparse matrix from the Matrix package and produce a
data.frame/tibble with sparse columns. The last one takes a
data.frame/tibble with sparse columns and produces a sparse matrix using
the Matrix package.

These functions are expected to be inverse of each other, such that
`coerce_to_sparse_matrix(coerce_to_sparse_data_frame(x))` returns `x`
back. They are made to be highly performant both in terms of speed and
memory consumption, Meaning that sparsity is applied when appropriate.

These functions have quite strict input checking by choice, to allow the
inner workings to be as efficient as possible. It is in part why
data.frames with sparse vectors with different can’t be used with
[`coerce_to_sparse_matrix()`](https://r-lib.github.io/sparsevctrs/reference/coerce_to_sparse_matrix.md)
yet.

## Helper Functions

There are 3 types of helper functions. First, we have the `is_*` family
of functions. The specific
[`is_sparse_double()`](https://r-lib.github.io/sparsevctrs/reference/type-predicates.md)
and more general
[`is_sparse_vector()`](https://r-lib.github.io/sparsevctrs/reference/type-predicates.md)
can be used as a way to determine whether a vector is an ALTREP sparse
vector. This is otherwise hard to tell as
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) can’t tell the
difference.

Secondly, we have the extraction functions. They are
[`sparse_values()`](https://r-lib.github.io/sparsevctrs/reference/extractors.md)
and
[`sparse_positions()`](https://r-lib.github.io/sparsevctrs/reference/extractors.md).
These extract the values and positions respectively, without
materializing the whole dense vector. These functions are made to work
with non-sparse vectors as well to make them more ergonomic for the
user. Internally they call
[`is_sparse_vector()`](https://r-lib.github.io/sparsevctrs/reference/type-predicates.md),
so the choice to return something useful as the alternative wasn’t hard.
There is no `sparse_length()` function as
[`length()`](https://rdrr.io/r/base/length.html) works with these types
of

The last type of helper function is less clearly defined and is expanded
as needed. The functions provide alternatives to functions that don’t
have ALTREP support. Such as
[`mean()`](https://rdrr.io/r/base/mean.html). Calling
[`mean()`](https://rdrr.io/r/base/mean.html) on a sparse vector will
force materialization, and then calculate the mean. This is memory
inefficient as it could have been calculated like so.

``` r
sum(sparse_values(x)) / length(x)
```

These functions, all starting with the name prefix `sparse_*`, are made
to work with non-sparse vectors for the same reasons listed above
regarding ergonomic use.

## FAQ

> Why aren’t the results returned as {vctrs} classes?

As it stands right now, it is viewed to be beneficial to have the users
not be alerted to these vectors as they are expected to be used
internally in packages and rarely by the end user. Furthermore having
these sparse vectors produce the same result as dense vectors with
[`class()`](https://rdrr.io/r/base/class.html) is a big plus.

> Will this package try to replace the {Matrix} package?

Not at all. The sparse vector types provided in this package mimic those
created with
[`Matrix::sparseVector()`](https://rdrr.io/pkg/Matrix/man/sparseVector.html).
They work with different types and allow for different defaults. None of
the matrix operations will be reimplemented here.
