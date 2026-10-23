#include "sparsevctrs-core.h"

#include "sparse-extractors.h"

SEXP ffi_sparse_altrep_sparse_positions(SEXP x) {
  SEXP out = sparse_extract_pos(x);
  return out;
}

SEXP ffi_sparse_altrep_sparse_values(SEXP x) {
  SEXP out = sparse_extract_val(x);
  return out;
}

SEXP ffi_sparse_altrep_sparse_default(SEXP x) {
  SEXP out = sparse_extract_default(x);
  return out;
}
