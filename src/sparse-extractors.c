#include "sparse-extractors.h"

SEXP ffi_altrep_sparse_positions(SEXP x) {
  SEXP out = extract_pos(x);
  return out;
}

SEXP ffi_altrep_sparse_values(SEXP x) {
  SEXP out = extract_val(x);
  return out;
}

SEXP ffi_altrep_sparse_default(SEXP x) {
  SEXP out = extract_default(x);
  return out;
}
