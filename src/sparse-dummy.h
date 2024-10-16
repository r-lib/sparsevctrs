#ifndef SPARSEVCTRS_SPARSE_DUMMY_H
#define SPARSEVCTRS_SPARSE_DUMMY_H

#define R_NO_REMAP
#include <Rinternals.h>
#include "sparse-utils.h"

SEXP ffi_sparse_dummy(SEXP x, SEXP lvls, SEXP counts);

#endif
