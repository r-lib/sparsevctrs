#ifndef SPARSEVCTRS_ALTREP_SPARSE_COMMON_H
#define SPARSEVCTRS_ALTREP_SPARSE_COMMON_H

#include "sparse-utils.h"

typedef r_obj* (*sparse_new_altrep_fn)(r_obj* data1);

r_obj* sparse_new(
  r_obj* val,
  r_obj* pos,
  r_obj* len,
  r_obj* def,
  sparse_new_altrep_fn new_altrep
);

const void* sparse_dataptr_or_null(r_obj* x);

Rboolean sparse_inspect(r_obj* x, const char* class_name);

r_obj* sparse_duplicate(
  r_obj* x,
  Rboolean deep,
  sparse_new_altrep_fn new_altrep
);

r_obj* sparse_subset_matches(r_obj* x, r_obj* indx, r_ssize* p_n_hits);

r_obj* sparse_subset_list(r_obj* x, r_ssize size, r_obj* out_val);

int sparse_int_is_sorted(r_obj* x, const int* v_val, int v_default_val);

int sparse_int_no_na(r_obj* x, const int* v_val);

static inline r_ssize sparse_elt_loc(r_obj* x, r_ssize i) {
  if (i >= sparse_extract_len(x)) {
    return -1;
  }

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);
  const r_ssize n_pos = r_length(pos);

  // TODO: Add `r_xlen_t_to_int()`
  const int needle = (int) i + 1;

  return sparse_binary_search(needle, v_pos, n_pos);
}

#endif
