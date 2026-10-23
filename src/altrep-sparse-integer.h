#ifndef SPARSEVCTRS_ALTREP_SPARSE_INTEGER
#define SPARSEVCTRS_ALTREP_SPARSE_INTEGER

#include "sparse-utils.h"

SEXP new_altrep_sparse_integer(SEXP data1);

SEXP new_sparse_integer(SEXP val, SEXP pos, SEXP len, SEXP def);

#endif
