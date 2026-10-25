#include "sparsevctrs-core.h"

#include "sparse-utils.h"
#include "altrep-sparse-integer.h"
#include "altrep-sparse-common.h"

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
  return sparse_new(val, pos, len, def, new_altrep_sparse_integer);
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
  return sparse_dataptr_or_null(x);
}

static r_obj* altrep_sparse_integer_Extract_subset(
  r_obj* x,
  r_obj* indx,
  r_obj* call
) {
  if (!sparse_is_index_handleable(indx)) {
    return NULL;
  }

  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_int_cbegin(val);

  const r_ssize n_pos = r_length(sparse_extract_pos(x));

  const r_ssize size = r_length(indx);

  r_ssize n_hits = 0;
  r_obj* matches = KEEP(sparse_subset_matches(x, indx, &n_hits));
  const int* v_matches = r_int_cbegin(matches);

  r_obj* out_val = KEEP(r_alloc_integer(n_hits));
  int* v_out_val = r_int_begin(out_val);

  r_obj* out = KEEP(sparse_subset_list(x, size, out_val));
  int* v_out_pos = r_int_begin(r_list_get(out, 1));

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

  FREE(3);
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
  return sparse_inspect(x, "altrep_sparse_integer");
}

static r_obj* altrep_sparse_integer_Duplicate(r_obj* x, Rboolean deep) {
  return sparse_duplicate(x, deep, new_altrep_sparse_integer);
}

// -----------------------------------------------------------------------------
// ALTINTEGER

static int altrep_sparse_integer_Elt(r_obj* x, r_ssize i) {
  r_obj* data2 = R_altrep_data2(x);

  if (data2 != r_null) {
    return r_int_get(data2, i);
  }

  const r_ssize loc = sparse_elt_loc(x, i);

  if (loc < 0) {
    // OOB of vector itself
    return r_globals.na_int;
  }

  if (loc == r_length(sparse_extract_pos(x))) {
    // Can't find it, must be the default value
    return sparse_extract_default_integer(x);
  } else {
    // Look it up in `val`
    return r_int_get(sparse_extract_val(x), loc);
  }
}

static int altrep_sparse_integer_Is_sorted(r_obj* x) {
  const int* v_val = r_int_cbegin(sparse_extract_val(x));

  return sparse_int_is_sorted(x, v_val, sparse_extract_default_integer(x));
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
  const int* v_val = r_int_cbegin(sparse_extract_val(x));

  return sparse_int_no_na(x, v_val);
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
