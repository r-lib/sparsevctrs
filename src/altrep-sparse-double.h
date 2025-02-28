#ifndef SPARSEVCTRS_ALTREP_SPARSE_DOUBLE
#define SPARSEVCTRS_ALTREP_SPARSE_DOUBLE

#define R_NO_REMAP
#include <Rinternals.h>
#include "sparse-utils.h"

SEXP new_sparse_double(SEXP val, SEXP pos, SEXP len, SEXP def);

#endif
