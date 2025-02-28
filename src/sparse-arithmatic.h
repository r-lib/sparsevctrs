#ifndef SPARSEVCTRS_SPARSE_ARITHMATIC_H
#define SPARSEVCTRS_SPARSE_ARITHMATIC_H

#define R_NO_REMAP
#include <Rinternals.h>
#include "sparse-utils.h"

SEXP ffi_sparse_multiplication(SEXP x, SEXP y);

#endif
