#ifndef SPARSEVCTRS_SPARSE_EXTRACTORS_H
#define SPARSEVCTRS_SPARSE_EXTRACTORS_H

#include "sparse-utils.h"

SEXP ffi_sparse_altrep_sparse_positions(SEXP x);

SEXP ffi_sparse_altrep_sparse_values(SEXP x);

SEXP ffi_sparse_altrep_sparse_default(SEXP x);

#endif
