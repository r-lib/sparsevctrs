#include "sparsevctrs-core.h"

#include "altrep-sparse-common.h"

#include "decl/altrep-sparse-common-decl.h"

r_obj* sparse_new(
  r_obj* val,
  r_obj* pos,
  r_obj* len,
  r_obj* def,
  sparse_new_altrep_fn new_altrep
) {
  r_obj* list = KEEP(r_alloc_list(4));

  r_list_poke(list, 0, val);
  r_list_poke(list, 1, pos);
  r_list_poke(list, 2, len);
  r_list_poke(list, 3, def);

  r_obj* out = new_altrep(list);

  FREE(1);
  return out;
}

const void* sparse_dataptr_or_null(r_obj* x) {
  r_obj* out = R_altrep_data2(x);

  if (out == r_null) {
    return NULL;
  } else {
    return DATAPTR_RW(out);
  }
}

void sparse_sync(r_obj* x) {
  r_obj* data2 = R_altrep_data2(x);

  if (data2 == r_null) {
    return;
  }

  r_obj* data1 = R_altrep_data1(x);

  if (r_length(data1) == 4) {
    return;
  }

  if (r_int_get(r_list_get(data1, 4), 0) == 0) {
    return;
  }

  r_obj* pos = KEEP(sparse_materialized_pos(x, data2));
  r_obj* val = KEEP(sparse_materialized_val(x, data2, pos));

  r_obj* out = KEEP(r_alloc_list(5));
  r_list_poke(out, 0, val);
  r_list_poke(out, 1, pos);
  r_list_poke(out, 2, r_list_get(data1, 2));
  r_list_poke(out, 3, sparse_extract_default(x));
  r_list_poke(out, 4, r_int(0));

  R_set_altrep_data1(x, out);

  FREE(3);
}

void sparse_mark_dirty(r_obj* x) {
  r_obj* data1 = R_altrep_data1(x);

  if (r_length(data1) == 5) {
    r_int_begin(r_list_get(data1, 4))[0] = 1;
    return;
  }

  r_obj* out = KEEP(r_alloc_list(5));
  for (r_ssize i = 0; i < 4; ++i) {
    r_list_poke(out, i, r_list_get(data1, i));
  }
  r_list_poke(out, 4, r_int(1));

  R_set_altrep_data1(x, out);

  FREE(1);
}

static r_obj* sparse_materialized_pos(r_obj* x, r_obj* data2) {
  r_obj* old_pos = r_list_get(R_altrep_data1(x), 1);
  const int* v_old_pos = r_int_cbegin(old_pos);
  const r_ssize n_old_pos = r_length(old_pos);

  const r_ssize n_out =
    sparse_materialized_pos_scan(x, data2, v_old_pos, n_old_pos, NULL);

  r_obj* out = KEEP(r_alloc_integer(n_out));
  int* v_out = r_int_begin(out);

  sparse_materialized_pos_scan(x, data2, v_old_pos, n_old_pos, v_out);

  FREE(1);
  return out;
}

static r_ssize sparse_materialized_pos_scan(
  r_obj* x,
  r_obj* data2,
  const int* v_old_pos,
  r_ssize n_old_pos,
  int* v_out
) {
  const r_ssize len = r_length(data2);
  r_ssize j = 0;
  r_ssize n_out = 0;

  switch (r_typeof(data2)) {
  case R_TYPE_double: {
    const double* v_data2 = r_dbl_cbegin(data2);
    const double default_val = sparse_extract_default_double(x);

    for (r_ssize i = 0; i < len; ++i) {
      const bool stored = sparse_pos_is_stored(v_old_pos, n_old_pos, &j, i);

      if (stored || !sparse_double_equal(v_data2[i], default_val)) {
        if (v_out != NULL) {
          v_out[n_out] = (int) (i + 1);
        }
        ++n_out;
      }
    }
    break;
  }
  case R_TYPE_integer: {
    const int* v_data2 = r_int_cbegin(data2);
    const int default_val = sparse_extract_default_integer(x);

    for (r_ssize i = 0; i < len; ++i) {
      const bool stored = sparse_pos_is_stored(v_old_pos, n_old_pos, &j, i);

      if (stored || v_data2[i] != default_val) {
        if (v_out != NULL) {
          v_out[n_out] = (int) (i + 1);
        }
        ++n_out;
      }
    }
    break;
  }
  case R_TYPE_logical: {
    const int* v_data2 = r_lgl_cbegin(data2);
    const int default_val = sparse_extract_default_logical(x);

    for (r_ssize i = 0; i < len; ++i) {
      const bool stored = sparse_pos_is_stored(v_old_pos, n_old_pos, &j, i);

      if (stored || v_data2[i] != default_val) {
        if (v_out != NULL) {
          v_out[n_out] = (int) (i + 1);
        }
        ++n_out;
      }
    }
    break;
  }
  case R_TYPE_character: {
    r_obj* default_val = sparse_extract_default_string(x);

    for (r_ssize i = 0; i < len; ++i) {
      const bool stored = sparse_pos_is_stored(v_old_pos, n_old_pos, &j, i);

      if (stored || !sparse_string_equal(r_chr_get(data2, i), default_val)) {
        if (v_out != NULL) {
          v_out[n_out] = (int) (i + 1);
        }
        ++n_out;
      }
    }
    break;
  }
  default:
    r_stop_unreachable();
  }

  return n_out;
}

static inline bool sparse_pos_is_stored(
  const int* v_pos,
  r_ssize n_pos,
  r_ssize* p_j,
  r_ssize i
) {
  if (*p_j < n_pos && v_pos[*p_j] == i + 1) {
    ++(*p_j);
    return true;
  }
  return false;
}

static inline bool sparse_double_equal(double x, double y) {
  if (isnan(x) || isnan(y)) {
    return isnan(x) && isnan(y) && R_IsNA(x) == R_IsNA(y);
  }
  return x == y;
}

static inline bool sparse_string_equal(r_obj* x, r_obj* y) {
  return x == y;
}

static r_obj* sparse_materialized_val(r_obj* x, r_obj* data2, r_obj* pos) {
  const int* v_pos = r_int_cbegin(pos);
  const r_ssize n_pos = r_length(pos);

  r_obj* out = KEEP(r_alloc_vector(r_typeof(data2), n_pos));

  switch (r_typeof(data2)) {
  case R_TYPE_double: {
    const double* v_data2 = r_dbl_cbegin(data2);
    double* v_out = r_dbl_begin(out);

    for (r_ssize i = 0; i < n_pos; ++i) {
      v_out[i] = v_data2[v_pos[i] - 1];
    }
    break;
  }
  case R_TYPE_integer: {
    const int* v_data2 = r_int_cbegin(data2);
    int* v_out = r_int_begin(out);

    for (r_ssize i = 0; i < n_pos; ++i) {
      v_out[i] = v_data2[v_pos[i] - 1];
    }
    break;
  }
  case R_TYPE_logical: {
    const int* v_data2 = r_lgl_cbegin(data2);
    int* v_out = r_lgl_begin(out);

    for (r_ssize i = 0; i < n_pos; ++i) {
      v_out[i] = v_data2[v_pos[i] - 1];
    }
    break;
  }
  case R_TYPE_character: {
    for (r_ssize i = 0; i < n_pos; ++i) {
      r_chr_poke(out, i, r_chr_get(data2, v_pos[i] - 1));
    }
    break;
  }
  default:
    r_stop_unreachable();
  }

  FREE(1);
  return out;
}

Rboolean sparse_inspect(r_obj* x, const char* class_name) {
  Rprintf(
    "sparsevctrs_%s (materialized=%s, length=%i)\n",
    class_name,
    R_altrep_data2(x) != r_null ? "T" : "F",
    (int) sparse_extract_len(x)
  );
  return TRUE;
}

r_obj* sparse_duplicate(
  r_obj* x,
  Rboolean deep,
  sparse_new_altrep_fn new_altrep
) {
  r_obj* data1 = R_altrep_data1(x);
  r_obj* data2 = R_altrep_data2(x);

  /* If deep or already materialized, do the default behavior */
  if (deep || data2 != r_null) {
    return NULL;
  }

  return new_altrep(data1);
}

r_obj* sparse_subset_matches(r_obj* x, r_obj* indx, r_ssize* p_n_hits) {
  const r_ssize len = sparse_extract_len(x);

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);
  const r_ssize n_pos = r_length(pos);

  const int* v_indx = r_int_cbegin(indx);

  const r_ssize size = r_length(indx);

  r_ssize n_hits = 0;
  r_obj* matches = r_alloc_integer(size);
  int* v_matches = r_int_begin(matches);

  for (r_ssize i = 0; i < size; ++i) {
    // 1 indexed!
    const int index = v_indx[i];

    if (index == r_globals.na_int) {
      v_matches[i] = r_globals.na_int;
      ++n_hits;
      continue;
    }

    if (index > len) {
      // (Uses `>` not `>=` because `index` is 1 indexed)
      // OOB
      v_matches[i] = r_globals.na_int;
      ++n_hits;
      continue;
    }

    const r_ssize loc = sparse_binary_search(index, v_pos, n_pos);

    if (loc == n_pos) {
      // Not in `pos`, gets default value
      v_matches[i] = (int) n_pos;
      continue;
    }

    // Did find in `pos`
    v_matches[i] = (int) loc;
    ++n_hits;
  }

  *p_n_hits = n_hits;
  return matches;
}

r_obj* sparse_subset_list(r_obj* x, r_ssize size, r_obj* out_val) {
  r_obj* out = KEEP(r_alloc_list(4));

  r_list_poke(out, 0, out_val);

  r_obj* out_pos = r_alloc_integer(r_length(out_val));
  r_list_poke(out, 1, out_pos);

  r_obj* out_length = r_int((int) size);
  r_list_poke(out, 2, out_length);

  r_obj* out_default = sparse_extract_default(x);
  r_list_poke(out, 3, out_default);

  FREE(1);
  return out;
}

int sparse_int_is_sorted(r_obj* x, const int* v_val, int v_default_val) {
  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize pos_len = r_length(pos);

  // zero length vector are by def sorted
  if (pos_len == 0) {
    return TRUE;
  }

  int current_value;

  if (v_pos[0] == 1) {
    current_value = v_val[0];
  } else {
    current_value = v_default_val;
  }

  for (r_ssize i = 0; i < pos_len; i++) {
    if (v_val[i] == r_globals.na_int) {
      return FALSE;
    }

    if (v_val[i] < current_value) {
      return FALSE;
    }

    current_value = v_val[i];

    if (i + 1 == pos_len) {
      break;
    }

    // If there is a gap between values check against default
    if ((v_pos[i + 1] - v_pos[i]) > 1) {
      if (v_default_val < current_value) {
        return FALSE;
      }

      current_value = v_default_val;
    }
  }

  // Trailing default values after the last stored value
  if (v_pos[pos_len - 1] < sparse_extract_len(x)) {
    if (v_default_val < current_value) {
      return FALSE;
    }
  }

  return TRUE;
}

int sparse_int_no_na(r_obj* x, const int* v_val) {
  const r_ssize val_len = r_length(sparse_extract_val(x));

  for (r_ssize i = 0; i < val_len; i++) {
    if (v_val[i] == r_globals.na_int) {
      return FALSE;
    }
  }

  return TRUE;
}
