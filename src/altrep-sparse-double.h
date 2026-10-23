#ifndef SPARSEVCTRS_ALTREP_SPARSE_DOUBLE
#define SPARSEVCTRS_ALTREP_SPARSE_DOUBLE

#include "sparse-utils.h"

SEXP new_altrep_sparse_double(SEXP data1);

SEXP new_sparse_double(SEXP val, SEXP pos, SEXP len, SEXP def);

#endif
