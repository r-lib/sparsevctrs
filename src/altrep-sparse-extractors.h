#ifndef SPARSEVCTRS_SPARSE_EXTRACTORS_H
#define SPARSEVCTRS_SPARSE_EXTRACTORS_H

#include <Rinternals.h>

SEXP ffi_altrep_sparse_positions(SEXP x);

SEXP ffi_altrep_sparse_values(SEXP x);

#endif
