#ifndef SPARSEVCTRS_ALTREP_SPARSE_INTEGER
#define SPARSEVCTRS_ALTREP_SPARSE_INTEGER

#define R_NO_REMAP
#include <Rinternals.h>
#include "sparse-utils.h"

SEXP new_sparse_integer(SEXP val, SEXP pos, SEXP len, SEXP def);

#endif
