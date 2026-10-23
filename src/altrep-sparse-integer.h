#ifndef SPARSEVCTRS_ALTREP_SPARSE_INTEGER
#define SPARSEVCTRS_ALTREP_SPARSE_INTEGER

#include "sparse-utils.h"

r_obj* new_altrep_sparse_integer(r_obj* data1);

r_obj* new_sparse_integer(r_obj* val, r_obj* pos, r_obj* len, r_obj* def);

#endif
