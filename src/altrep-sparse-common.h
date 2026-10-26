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

static inline void sparse_int_materialize(
  int* v_out,
  r_ssize len,
  int default_val,
  const int* v_val,
  const int* v_pos,
  r_ssize n_pos
) {
  for (r_ssize i = 0; i < len; ++i) {
    v_out[i] = default_val;
  }

  for (r_ssize i = 0; i < n_pos; ++i) {
    const int loc = v_pos[i] - 1;
    v_out[loc] = v_val[i];
  }
}

static inline void sparse_dbl_materialize(
  double* v_out,
  r_ssize len,
  double default_val,
  const double* v_val,
  const int* v_pos,
  r_ssize n_pos
) {
  for (r_ssize i = 0; i < len; ++i) {
    v_out[i] = default_val;
  }

  for (r_ssize i = 0; i < n_pos; ++i) {
    const int loc = v_pos[i] - 1;
    v_out[loc] = v_val[i];
  }
}

static inline void sparse_chr_materialize(
  r_obj* out,
  r_ssize len,
  r_obj* default_val,
  r_obj* val,
  const int* v_pos,
  r_ssize n_pos
) {
  for (r_ssize i = 0; i < len; ++i) {
    r_chr_poke(out, i, default_val);
  }

  for (r_ssize i = 0; i < n_pos; ++i) {
    const int loc = v_pos[i] - 1;
    r_chr_poke(out, loc, r_chr_get(val, i));
  }
}

static inline void sparse_int_subset(
  const int* v_val,
  const int* v_matches,
  r_ssize size,
  r_ssize n_pos,
  int na,
  int* v_out_val,
  int* v_out_pos
) {
  r_ssize i_out = 0;

  for (r_ssize i = 0; i < size; ++i) {
    const int match = v_matches[i];

    if (match == (int) n_pos) {
      // Default value case
      continue;
    }

    if (match == r_globals.na_int) {
      v_out_val[i_out] = na;
    } else {
      // Otherwise we have a hit from `pos`
      v_out_val[i_out] = v_val[match];
    }

    v_out_pos[i_out] = (int) i + 1;
    ++i_out;
  }
}

static inline void sparse_dbl_subset(
  const double* v_val,
  const int* v_matches,
  r_ssize size,
  r_ssize n_pos,
  double* v_out_val,
  int* v_out_pos
) {
  r_ssize i_out = 0;

  for (r_ssize i = 0; i < size; ++i) {
    const int match = v_matches[i];

    if (match == (int) n_pos) {
      // Default value case
      continue;
    }

    if (match == r_globals.na_int) {
      v_out_val[i_out] = r_globals.na_dbl;
    } else {
      // Otherwise we have a hit from `pos`
      v_out_val[i_out] = v_val[match];
    }

    v_out_pos[i_out] = (int) i + 1;
    ++i_out;
  }
}

static inline void sparse_chr_subset(
  r_obj* val,
  const int* v_matches,
  r_ssize size,
  r_ssize n_pos,
  r_obj* out_val,
  int* v_out_pos
) {
  r_ssize i_out = 0;

  for (r_ssize i = 0; i < size; ++i) {
    const int match = v_matches[i];

    if (match == (int) n_pos) {
      // Default value case
      continue;
    }

    if (match == r_globals.na_int) {
      r_chr_poke(out_val, i_out, r_globals.na_str);
    } else {
      // Otherwise we have a hit from `pos`
      r_chr_poke(out_val, i_out, r_chr_get(val, match));
    }

    v_out_pos[i_out] = (int) i + 1;
    ++i_out;
  }
}

#endif
