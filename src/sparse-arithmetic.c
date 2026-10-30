#include "sparsevctrs-core.h"

#include "sparse-arithmetic.h"
#include "sparse-utils.h"
#include "altrep-sparse-double.h"
#include "altrep-sparse-integer.h"

#include "decl/sparse-arithmetic-decl.h"

r_obj* ffi_sparse_multiplication(r_obj* ffi_x, r_obj* ffi_y) {
  return multiplication(ffi_x, ffi_y);
}

static r_obj* multiplication(r_obj* x, r_obj* y) {
  const bool x_sparse = sparse_is_sparse_vector(x);
  const bool y_sparse = sparse_is_sparse_vector(y);

  if (x_sparse) {
    if (y_sparse) {
      return multiplication_sparse_sparse(x, y);
    } else {
      return multiplication_sparse_dense(x, y);
    }
  } else {
    if (y_sparse) {
      return multiplication_sparse_dense(y, x);
    } else {
      return multiplication_dense_dense(x, y);
    }
  }
}

static r_obj* multiplication_sparse_sparse(r_obj* x, r_obj* y) {
  const bool is_integer = r_typeof(x) == R_TYPE_integer;

  r_obj* overlap = KEEP(find_overlap(x, y));
  r_obj* nas = KEEP(find_nas_with_no_overlap(x, y));

  r_obj* x_val = sparse_extract_val(x);
  r_obj* x_pos = sparse_extract_pos(x);
  r_obj* y_val = sparse_extract_val(y);
  r_obj* y_pos = sparse_extract_pos(y);

  const void* v_x_val = r_vec_cbegin(x_val);
  const int* v_x_pos = r_int_cbegin(x_pos);
  const void* v_y_val = r_vec_cbegin(y_val);
  const int* v_y_pos = r_int_cbegin(y_pos);

  r_obj* x_na_pos = r_list_get(nas, 0);
  r_obj* y_na_pos = r_list_get(nas, 1);

  const int* v_x_na_pos = r_int_cbegin(x_na_pos);
  const int* v_y_na_pos = r_int_cbegin(y_na_pos);

  const r_ssize x_na_count = r_length(x_na_pos);
  const r_ssize y_na_count = r_length(y_na_pos);

  if (overlap == r_null && x_na_count == 0 && y_na_count == 0) {
    FREE(2);
    return empty_sparse(sparse_extract_len(x), is_integer);
  }

  r_ssize n_overlap = 0;
  if (overlap != r_null) {
    n_overlap = r_length(r_list_get(overlap, 0));
  }

  const r_ssize out_len = n_overlap + x_na_count + y_na_count;

  r_obj* out_pos = KEEP(r_alloc_integer(out_len));
  int* v_out_pos = r_int_begin(out_pos);
  r_obj* out_val = KEEP(alloc_vec(is_integer, out_len));
  void* v_out_val = r_vec_begin(out_val);
  r_ssize out_idx = 0;

  if (overlap != r_null) {
    const int* v_x_pos_idx = r_int_cbegin(r_list_get(overlap, 0));
    const int* v_y_pos_idx = r_int_cbegin(r_list_get(overlap, 1));

    for (r_ssize i = 0; i < n_overlap; i++) {
      v_out_pos[out_idx] = v_x_pos[v_x_pos_idx[i]];
      poke_product(
        v_out_val,
        out_idx,
        v_x_val,
        v_x_pos_idx[i],
        v_y_val,
        v_y_pos_idx[i],
        is_integer
      );
      out_idx++;
    }
  }

  // set x NA values
  for (r_ssize i = 0; i < x_na_count; i++) {
    v_out_pos[out_idx] = v_x_pos[v_x_na_pos[i]];
    poke_na(v_out_val, out_idx, is_integer);
    out_idx++;
  }

  // set y NA values
  for (r_ssize i = 0; i < y_na_count; i++) {
    v_out_pos[out_idx] = v_y_pos[v_y_na_pos[i]];
    poke_na(v_out_val, out_idx, is_integer);
    out_idx++;
  }

  sparse_sort_pos_and_val(out_pos, out_val);

  r_obj* out = new_sparse_product(out_val, out_pos, sparse_extract_len(x));

  FREE(4);

  return out;
}

static r_obj* multiplication_sparse_dense(r_obj* x, r_obj* y) {
  const bool is_integer = r_typeof(x) == R_TYPE_integer;

  r_obj* x_pos = sparse_extract_pos(x);
  r_obj* x_val = sparse_extract_val(x);
  const int* v_x_pos = r_int_cbegin(x_pos);
  const void* v_x_val = r_vec_cbegin(x_val);
  const void* v_y = r_vec_cbegin(y);
  const r_ssize x_len = sparse_extract_len(x);
  const r_ssize n_values = r_length(x_pos);

  r_ssize n_zero = 0;
  for (r_ssize i = 0; i < n_values; i++) {
    if (is_zero_at(v_y, v_x_pos[i] - 1, is_integer)) {
      n_zero++;
    }
  }

  // Locate NA values for Y - dense
  r_ssize n_y_nas = 0;
  const r_ssize y_len = r_length(y);
  for (r_ssize i = 0; i < y_len; i++) {
    if (is_na_at(v_y, i, is_integer)) {
      // i + 1 because of R-indexing
      if (!int_match((int) i + 1, x_pos)) {
        n_y_nas++;
      }
    }
  }

  r_obj* y_na_pos = KEEP(r_alloc_integer(n_y_nas));
  int* v_y_na_pos = r_int_begin(y_na_pos);
  r_ssize idx = 0;

  for (r_ssize i = 0; i < y_len; i++) {
    if (is_na_at(v_y, i, is_integer)) {
      // i + 1 because of R-indexing
      if (!int_match((int) i + 1, x_pos)) {
        v_y_na_pos[idx] = i;
        idx++;
      }
    }
  }

  // Locate NA values for X sparse
  r_ssize n_x_nas = 0;
  for (r_ssize i = 0; i < n_values; i++) {
    if (is_na_at(v_x_val, i, is_integer)) {
      // v_x_pos[i] - 1 because of R-indexing
      if (is_zero_at(v_y, v_x_pos[i] - 1, is_integer)) {
        n_x_nas++;
      }
    }
  }

  r_obj* x_na_pos = KEEP(r_alloc_integer(n_x_nas));
  int* v_x_na_pos = r_int_begin(x_na_pos);
  idx = 0;

  for (r_ssize i = 0; i < n_values; i++) {
    if (is_na_at(v_x_val, i, is_integer)) {
      const int cur_pos = v_x_pos[i];
      // cur_pos - 1 because of R-indexing
      if (is_zero_at(v_y, cur_pos - 1, is_integer)) {
        v_x_na_pos[idx] = cur_pos;
        idx++;
      }
    }
  }

  const r_ssize out_len = n_values - n_zero + n_x_nas + n_y_nas;
  r_obj* out_pos = KEEP(r_alloc_integer(out_len));
  int* v_out_pos = r_int_begin(out_pos);
  r_obj* out_val = KEEP(alloc_vec(is_integer, out_len));
  void* v_out_val = r_vec_begin(out_val);

  idx = 0;

  for (r_ssize i = 0; i < n_values; i++) {
    const int cur_pos = v_x_pos[i];

    if (!is_zero_at(v_y, cur_pos - 1, is_integer)) {
      v_out_pos[idx] = cur_pos;
      poke_product(v_out_val, idx, v_x_val, i, v_y, cur_pos - 1, is_integer);
      idx++;
    }
  }

  // set x na values
  for (r_ssize i = 0; i < n_x_nas; i++) {
    const int cur_pos = v_x_na_pos[i];

    v_out_pos[idx] = cur_pos;
    poke_na(v_out_val, idx, is_integer);
    idx++;
  }

  // set y na values
  for (r_ssize i = 0; i < n_y_nas; i++) {
    const int cur_pos = v_y_na_pos[i];

    // cur_pos + 1 because it is R-indexed
    v_out_pos[idx] = cur_pos + 1;
    poke_na(v_out_val, idx, is_integer);
    idx++;
  }

  sparse_sort_pos_and_val(out_pos, out_val);

  r_obj* out = new_sparse_product(out_val, out_pos, x_len);

  FREE(4);
  return out;
}

static r_obj* multiplication_dense_dense(r_obj* x, r_obj* y) {
  const bool is_integer = r_typeof(x) == R_TYPE_integer;

  const r_ssize len = r_length(x);

  r_obj* out = alloc_vec(is_integer, len);

  if (is_integer) {
    const int* v_x = r_int_cbegin(x);
    const int* v_y = r_int_cbegin(y);
    int* v_out = r_int_begin(out);

    for (r_ssize i = 0; i < len; i++) {
      poke_product(v_out, i, v_x, i, v_y, i, true);
    }
  } else {
    const double* v_x = r_dbl_cbegin(x);
    const double* v_y = r_dbl_cbegin(y);
    double* v_out = r_dbl_begin(out);

    for (r_ssize i = 0; i < len; i++) {
      poke_product(v_out, i, v_x, i, v_y, i, false);
    }
  }

  return out;
}

static inline bool is_na_at(const void* v, r_ssize i, bool is_integer) {
  if (is_integer) {
    return ((const int*) v)[i] == r_globals.na_int;
  }
  return R_IsNA(((const double*) v)[i]);
}

static inline bool is_zero_at(const void* v, r_ssize i, bool is_integer) {
  if (is_integer) {
    return ((const int*) v)[i] == 0;
  }
  return ((const double*) v)[i] == 0;
}

static inline void poke_na(void* v_out, r_ssize i_out, bool is_integer) {
  if (is_integer) {
    ((int*) v_out)[i_out] = r_globals.na_int;
  } else {
    ((double*) v_out)[i_out] = r_globals.na_dbl;
  }
}

static inline void poke_product(
  void* v_out,
  r_ssize i_out,
  const void* v_x,
  r_ssize i_x,
  const void* v_y,
  r_ssize i_y,
  bool is_integer
) {
  if (is_integer) {
    const int x_val = ((const int*) v_x)[i_x];
    const int y_val = ((const int*) v_y)[i_y];

    if (x_val == r_globals.na_int || y_val == r_globals.na_int) {
      ((int*) v_out)[i_out] = r_globals.na_int;
    } else {
      ((int*) v_out)[i_out] = x_val * y_val;
    }
  } else {
    ((double*) v_out)[i_out] =
      ((const double*) v_x)[i_x] * ((const double*) v_y)[i_y];
  }
}

static r_obj* alloc_vec(bool is_integer, r_ssize n) {
  if (is_integer) {
    return r_alloc_integer(n);
  }
  return r_alloc_double(n);
}

static r_obj* find_overlap(r_obj* x, r_obj* y) {
  r_obj* x_pos = sparse_extract_pos(x);
  r_obj* y_pos = sparse_extract_pos(y);
  const r_ssize x_len = r_length(x_pos);
  const r_ssize y_len = r_length(y_pos);

  // special case if x or y have length zero
  if (x_len == 0 || y_len == 0) {
    return r_null;
  }

  r_obj* x_matches = KEEP(r_alloc_integer(x_len));
  int* v_x_matches = r_int_begin(x_matches);
  r_obj* y_matches = KEEP(r_alloc_integer(y_len));
  int* v_y_matches = r_int_begin(y_matches);
  for (r_ssize i = 0; i < x_len; i++) {
    v_x_matches[i] = 0;
  }
  for (r_ssize i = 0; i < y_len; i++) {
    v_y_matches[i] = 0;
  }

  const int* x_pos_v = r_int_cbegin(x_pos);
  const int* y_pos_v = r_int_cbegin(y_pos);

  r_ssize i = 0, j = 0, n = 0;

  while (i < x_len && j < y_len) {
    if (x_pos_v[i] < y_pos_v[j]) {
      i++;
    } else if (x_pos_v[i] > y_pos_v[j]) {
      j++;
    } else {
      n++;
      v_x_matches[i] = 1;
      v_y_matches[j] = 1;

      i++;
      j++;
    }
  }

  if (n == 0) {
    FREE(2);
    return r_null;
  }

  r_obj* out = KEEP(r_alloc_list(2));

  r_obj* out_x = r_alloc_integer(n);
  r_list_poke(out, 0, out_x);
  int* v_out_x = r_int_begin(out_x);

  r_obj* out_y = r_alloc_integer(n);
  r_list_poke(out, 1, out_y);
  int* v_out_y = r_int_begin(out_y);

  r_ssize loc = 0;
  for (r_ssize i = 0; i < x_len; i++) {
    if (v_x_matches[i] == 1) {
      v_out_x[loc] = i;
      loc++;
    }
  }
  loc = 0;
  for (r_ssize i = 0; i < y_len; i++) {
    if (v_y_matches[i] == 1) {
      v_out_y[loc] = i;
      loc++;
    }
  }

  FREE(3);
  return out;
}

static r_obj* find_nas_with_no_overlap(r_obj* x, r_obj* y) {
  r_obj* x_pos = sparse_extract_pos(x);
  r_obj* x_val = sparse_extract_val(x);
  r_obj* y_pos = sparse_extract_pos(y);
  r_obj* y_val = sparse_extract_val(y);
  const r_ssize x_len = r_length(x_pos);
  const r_ssize y_len = r_length(y_pos);

  r_obj* x_matches = KEEP(r_alloc_integer(x_len));
  int* v_x_matches = r_int_begin(x_matches);
  r_obj* y_matches = KEEP(r_alloc_integer(y_len));
  int* v_y_matches = r_int_begin(y_matches);

  mark_nas(x_val, v_x_matches);
  mark_nas(y_val, v_y_matches);

  const int* x_pos_v = r_int_cbegin(x_pos);
  const int* y_pos_v = r_int_cbegin(y_pos);

  r_ssize i = 0, j = 0;

  while (i < x_len && j < y_len) {
    if (x_pos_v[i] < y_pos_v[j]) {
      i++;
    } else if (x_pos_v[i] > y_pos_v[j]) {
      j++;
    } else {
      v_x_matches[i] = 0;
      v_y_matches[j] = 0;

      i++;
      j++;
    }
  }

  r_obj* out = KEEP(r_alloc_list(2));
  r_ssize n_x = 0;
  for (r_ssize i = 0; i < x_len; i++) {
    if (v_x_matches[i] == 1) {
      n_x++;
    }
  }
  r_ssize n_y = 0;
  for (r_ssize i = 0; i < y_len; i++) {
    if (v_y_matches[i] == 1) {
      n_y++;
    }
  }

  r_obj* out_x = r_alloc_integer(n_x);
  r_list_poke(out, 0, out_x);
  int* v_out_x = r_int_begin(out_x);

  r_obj* out_y = r_alloc_integer(n_y);
  r_list_poke(out, 1, out_y);
  int* v_out_y = r_int_begin(out_y);

  r_ssize loc = 0;
  for (r_ssize i = 0; i < x_len; i++) {
    if (v_x_matches[i] == 1) {
      v_out_x[loc] = i;
      loc++;
    }
  }
  loc = 0;
  for (r_ssize i = 0; i < y_len; i++) {
    if (v_y_matches[i] == 1) {
      v_out_y[loc] = i;
      loc++;
    }
  }

  FREE(3);
  return out;
}

static bool int_match(int x, r_obj* table) {
  const r_ssize table_len = r_length(table);
  const int* v_table = r_int_cbegin(table);

  bool out = false;

  for (r_ssize i = 0; i < table_len; i++) {
    if (v_table[i] == x) {
      out = true;
      break;
    }
  }
  return out;
}

static void mark_nas(r_obj* val, int* v_matches) {
  const r_ssize n = r_length(val);
  const void* v_val = r_vec_cbegin(val);
  const bool is_integer = r_typeof(val) == R_TYPE_integer;

  for (r_ssize i = 0; i < n; i++) {
    v_matches[i] = is_na_at(v_val, i, is_integer) ? 1 : 0;
  }
}

static r_obj* new_sparse_product(r_obj* val, r_obj* pos, r_ssize len) {
  const bool is_integer = r_typeof(val) == R_TYPE_integer;

  r_obj* out_length = KEEP(r_int((int) len));
  r_obj* out_default = KEEP(is_integer ? r_int(0) : r_dbl(0));

  r_obj* out;
  if (is_integer) {
    out = new_sparse_integer(val, pos, out_length, out_default);
  } else {
    out = new_sparse_double(val, pos, out_length, out_default);
  }

  FREE(2);
  return out;
}

static r_obj* empty_sparse(r_ssize len, bool is_integer) {
  r_obj* val = KEEP(alloc_vec(is_integer, 0));
  r_obj* pos = KEEP(r_alloc_integer(0));

  r_obj* out = new_sparse_product(val, pos, len);

  FREE(2);
  return out;
}
