#ifndef SPARSEVCTRS_SPARSE_DUMMY_H
#define SPARSEVCTRS_SPARSE_DUMMY_H

#include "sparse-utils.h"

SEXP ffi_sparse_dummy(SEXP x, SEXP lvls, SEXP counts, SEXP one_hot);

SEXP ffi_sparse_dummy_na(SEXP x, SEXP lvls, SEXP counts, SEXP one_hot);

#endif
