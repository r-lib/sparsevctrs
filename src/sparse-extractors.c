#include "sparsevctrs-core.h"

#include "sparse-extractors.h"

r_obj* ffi_sparse_altrep_sparse_positions(r_obj* ffi_x) {
  return sparse_extract_pos(ffi_x);
}

r_obj* ffi_sparse_altrep_sparse_values(r_obj* ffi_x) {
  return sparse_extract_val(ffi_x);
}

r_obj* ffi_sparse_altrep_sparse_default(r_obj* ffi_x) {
  return sparse_extract_default(ffi_x);
}
