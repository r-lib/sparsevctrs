#include "sparsevctrs-core.h"

#include "sparse-utils.h"
#include "altrep-sparse-integer.h"

#include "decl/altrep-sparse-integer-decl.h"

// Initialised at load time

R_altrep_class_t altrep_sparse_integer_class;

r_obj* ffi_sparse_altrep_new_sparse_integer(r_obj* ffi_x) {
  return new_altrep_sparse_integer(ffi_x);
}

r_obj* ffi_sparse_altrep_sparse_integer_materialize(r_obj* ffi_x) {
  return altrep_sparse_integer_Materialize(ffi_x);
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_integer(DllInfo* dll) {
  altrep_sparse_integer_class =
    R_make_altinteger_class("altrep_sparse_integer", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Dataptr
  );
  R_set_altvec_Dataptr_or_null_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Dataptr_or_null
  );
  R_set_altvec_Extract_subset_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Extract_subset
  );

  // ALTREP
  R_set_altrep_Length_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Length
  );
  R_set_altrep_Inspect_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Inspect
  );
  R_set_altrep_Duplicate_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Duplicate
  );

  // ALTINTEGER
  R_set_altinteger_Elt_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Elt
  );
  R_set_altinteger_Is_sorted_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Is_sorted
  );
  R_set_altinteger_Min_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Min_method
  );
  R_set_altinteger_Max_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Max_method
  );
  R_set_altinteger_No_NA_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_No_NA_method
  );
  R_set_altinteger_Sum_method(
    altrep_sparse_integer_class,
    altrep_sparse_integer_Sum_method
  );
}

r_obj* new_altrep_sparse_integer(r_obj* data1) {
  return R_new_altrep(altrep_sparse_integer_class, data1, r_null);
}

r_obj* new_sparse_integer(r_obj* val, r_obj* pos, r_obj* len, r_obj* def) {
  r_obj* list = KEEP(r_alloc_list(4));

  r_list_poke(list, 0, val);
  r_list_poke(list, 1, pos);
  r_list_poke(list, 2, len);
  r_list_poke(list, 3, def);

  r_obj* out = new_altrep_sparse_integer(list);

  FREE(1);
  return out;
}

static r_obj* altrep_sparse_integer_Materialize(r_obj* x) {
  r_obj* out = R_altrep_data2(x);

  if (out != r_null) {
    return out;
  }

  sparse_verbose_materialize();

  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_int_cbegin(val);

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize len = sparse_extract_len(x);

  const int v_default_val = sparse_extract_default_integer(x);

  out = KEEP(r_alloc_integer(len));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < len; ++i) {
    v_out[i] = v_default_val;
  }

  const r_ssize n_positions = r_length(pos);

  for (r_ssize i = 0; i < n_positions; ++i) {
    const int loc = v_pos[i] - 1;
    v_out[loc] = v_val[i];
  }

  R_set_altrep_data2(x, out);

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------
// ALTVEC

static void* altrep_sparse_integer_Dataptr(r_obj* x, Rboolean writeable) {
  return DATAPTR_RW(altrep_sparse_integer_Materialize(x));
}

static const void* altrep_sparse_integer_Dataptr_or_null(r_obj* x) {
  r_obj* out = R_altrep_data2(x);

  if (out == r_null) {
    return NULL;
  } else {
    return DATAPTR_RW(out);
  }
}

static r_obj* altrep_sparse_integer_Extract_subset(
  r_obj* x,
  r_obj* indx,
  r_obj* call
) {
  if (!sparse_is_index_handleable(indx)) {
    return NULL;
  }

  const r_ssize len = sparse_extract_len(x);

  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_int_cbegin(val);

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);
  const r_ssize n_pos = r_length(pos);

  const int* v_indx = r_int_cbegin(indx);

  const r_ssize size = r_length(indx);

  r_ssize n_hits = 0;
  r_obj* matches = KEEP(r_alloc_integer(size));
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

  r_obj* out = KEEP(r_alloc_list(4));

  r_obj* out_val = r_alloc_integer(n_hits);
  r_list_poke(out, 0, out_val);
  int* v_out_val = r_int_begin(out_val);

  r_obj* out_pos = r_alloc_integer(n_hits);
  r_list_poke(out, 1, out_pos);
  int* v_out_pos = r_int_begin(out_pos);

  r_obj* out_length = r_int((int) size);
  r_list_poke(out, 2, out_length);

  r_obj* out_default = sparse_extract_default(x);
  r_list_poke(out, 3, out_default);

  r_ssize i_out = 0;

  for (r_ssize i = 0; i < size; ++i) {
    const int match = v_matches[i];

    if (match == (int) n_pos) {
      // Default value case
      continue;
    }

    if (match == r_globals.na_int) {
      v_out_val[i_out] = r_globals.na_int;
      v_out_pos[i_out] = (int) i + 1;
      ++i_out;
      continue;
    }

    // Otherwise we have a hit from `pos`
    v_out_val[i_out] = v_val[match];
    v_out_pos[i_out] = (int) i + 1;
    ++i_out;
  }

  r_obj* altrep = new_altrep_sparse_integer(out);

  FREE(2);
  return altrep;
}

// -----------------------------------------------------------------------------
// ALTREP

static r_ssize altrep_sparse_integer_Length(r_obj* x) {
  const r_ssize out = sparse_extract_len(x);

  return out;
}

// What gets printed when .Internal(inspect()) is used
static Rboolean altrep_sparse_integer_Inspect(
  r_obj* x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(r_obj*, int, int, int)
) {
  Rprintf(
    "sparsevctrs_altrep_sparse_integer (materialized=%s, length=%i)\n",
    R_altrep_data2(x) != r_null ? "T" : "F",
    (int) sparse_extract_len(x)
  );
  return TRUE;
}

static r_obj* altrep_sparse_integer_Duplicate(r_obj* x, Rboolean deep) {
  r_obj* data1 = R_altrep_data1(x);
  r_obj* data2 = R_altrep_data2(x);

  /* If deep or already materialized, do the default behavior */
  if (deep || data2 != r_null) {
    return NULL;
  }

  return new_altrep_sparse_integer(data1);
}

// -----------------------------------------------------------------------------
// ALTINTEGER

static int altrep_sparse_integer_Elt(r_obj* x, r_ssize i) {
  r_obj* data2 = R_altrep_data2(x);

  if (data2 != r_null) {
    return r_int_get(data2, i);
  }

  r_obj* val = sparse_extract_val(x);

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);
  const r_ssize size = r_length(pos);

  const r_ssize len = sparse_extract_len(x);

  const int v_default_val = sparse_extract_default_integer(x);

  if (i > len) {
    // OOB of vector itself
    return r_globals.na_int;
  }

  // TODO: Add `r_xlen_t_to_int()`
  const int needle = (int) i + 1;
  const r_ssize loc = sparse_binary_search(needle, v_pos, size);

  if (loc == size) {
    // Can't find it, must be the default value
    return v_default_val;
  } else {
    // Look it up in `val`
    return r_int_get(val, loc);
  }
}

static int altrep_sparse_integer_Is_sorted(r_obj* x) {
  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize pos_len = r_length(pos);

  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_int_cbegin(val);

  const int v_default_val = sparse_extract_default_integer(x);

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

static r_obj* altrep_sparse_integer_Min_method(r_obj* x, Rboolean na_rm) {
  int min = INT_MAX;

  if (sparse_extract_len(x) == 0) {
    Rf_warning("no non-missing arguments to min; returning Inf");
    return r_dbl(R_PosInf);
  }

  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_int_cbegin(val);
  const r_ssize val_len = r_length(val);

  const int v_default_val = sparse_extract_default_integer(x);

  // The default only appears if some positions are not explicitly stored
  if (val_len < sparse_extract_len(x) && v_default_val < min) {
    min = v_default_val;
  }

  for (r_ssize i = 0; i < val_len; i++) {
    if (v_val[i] == r_globals.na_int) {
      if (na_rm) {
        continue;
      } else {
        return r_int(r_globals.na_int);
      }
    }

    if (v_val[i] < min) {
      min = v_val[i];
    }
  }
  return r_int(min);
}

static r_obj* altrep_sparse_integer_Max_method(r_obj* x, Rboolean na_rm) {
  int max = INT_MIN;

  if (sparse_extract_len(x) == 0) {
    Rf_warning("no non-missing arguments to max; returning -Inf");
    return r_dbl(R_NegInf);
  }

  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_int_cbegin(val);
  const r_ssize val_len = r_length(val);

  const int v_default_val = sparse_extract_default_integer(x);

  // The default only appears if some positions are not explicitly stored
  if (val_len < sparse_extract_len(x) && v_default_val > max) {
    max = v_default_val;
  }

  for (r_ssize i = 0; i < val_len; i++) {
    if (v_val[i] == r_globals.na_int) {
      if (na_rm) {
        continue;
      } else {
        return r_int(r_globals.na_int);
      }
    }

    if (v_val[i] > max) {
      max = v_val[i];
    }
  }
  return r_int(max);
}

static int altrep_sparse_integer_No_NA_method(r_obj* x) {
  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_int_cbegin(val);
  const r_ssize val_len = r_length(val);

  for (r_ssize i = 0; i < val_len; i++) {
    if (v_val[i] == r_globals.na_int) {
      return FALSE;
    }
  }

  return TRUE;
}

static r_obj* altrep_sparse_integer_Sum_method(r_obj* x, Rboolean na_rm) {
  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_int_cbegin(val);
  const r_ssize val_len = r_length(val);
  const r_ssize len = sparse_extract_len(x);

  int sum = 0;

  if (len == 0) {
    return r_int(sum);
  }

  for (r_ssize i = 0; i < val_len; i++) {
    if (v_val[i] == r_globals.na_int) {
      if (na_rm) {
        continue;
      } else {
        return r_int(r_globals.na_int);
      }
    }
    sum = sum + v_val[i];
  }

  // default can be non-zero
  const int v_default_val = sparse_extract_default_integer(x);

  if (v_default_val != 0) {
    sum = sum + (len - val_len) * v_default_val;
  }

  return r_int(sum);
}
