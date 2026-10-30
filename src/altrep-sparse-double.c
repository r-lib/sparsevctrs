#include "sparsevctrs-core.h"

#include "sparse-utils.h"
#include "altrep-sparse-double.h"
#include "altrep-sparse-common.h"

#include "decl/altrep-sparse-double-decl.h"

// Initialised at load time

R_altrep_class_t altrep_sparse_double_class;

r_obj* ffi_sparse_altrep_new_sparse_double(r_obj* ffi_x) {
  return new_altrep_sparse_double(ffi_x);
}

r_obj* ffi_sparse_altrep_sparse_double_materialize(r_obj* ffi_x) {
  return altrep_sparse_double_Materialize(ffi_x);
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_double(DllInfo* dll) {
  altrep_sparse_double_class =
    R_make_altreal_class("altrep_sparse_double", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Dataptr
  );
  R_set_altvec_Dataptr_or_null_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Dataptr_or_null
  );
  R_set_altvec_Extract_subset_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Extract_subset
  );

  // ALTREP
  R_set_altrep_Length_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Length
  );
  R_set_altrep_Inspect_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Inspect
  );
  R_set_altrep_Duplicate_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Duplicate
  );

  // ALTREAL
  R_set_altreal_Elt_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Elt
  );
  R_set_altreal_Is_sorted_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Is_sorted
  );
  R_set_altreal_Min_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Min_method
  );
  R_set_altreal_Max_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Max_method
  );
  R_set_altreal_No_NA_method(
    altrep_sparse_double_class,
    altrep_sparse_double_No_NA_method
  );
  R_set_altreal_Sum_method(
    altrep_sparse_double_class,
    altrep_sparse_double_Sum_method
  );
}

r_obj* new_altrep_sparse_double(r_obj* data1) {
  return R_new_altrep(altrep_sparse_double_class, data1, r_null);
}

r_obj* new_sparse_double(r_obj* val, r_obj* pos, r_obj* len, r_obj* def) {
  return sparse_new(val, pos, len, def, new_altrep_sparse_double);
}

static r_obj* altrep_sparse_double_Materialize(r_obj* x) {
  r_obj* out = R_altrep_data2(x);

  if (out != r_null) {
    return out;
  }

  sparse_verbose_materialize();

  r_obj* val = sparse_extract_val(x);
  const double* v_val = r_dbl_cbegin(val);

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize len = sparse_extract_len(x);

  const double v_default_val = sparse_extract_default_double(x);

  out = KEEP(r_alloc_double(len));
  double* v_out = r_dbl_begin(out);

  sparse_dbl_materialize(
    v_out,
    len,
    v_default_val,
    v_val,
    v_pos,
    r_length(pos)
  );

  R_set_altrep_data2(x, out);

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------
// ALTVEC

static void* altrep_sparse_double_Dataptr(r_obj* x, Rboolean writeable) {
  r_obj* out = altrep_sparse_double_Materialize(x);
  if (writeable) {
    sparse_mark_dirty(x);
  }
  return DATAPTR_RW(out);
}

static const void* altrep_sparse_double_Dataptr_or_null(r_obj* x) {
  return sparse_dataptr_or_null(x);
}

static r_obj* altrep_sparse_double_Extract_subset(
  r_obj* x,
  r_obj* indx,
  r_obj* call
) {
  if (!sparse_is_index_handleable(indx)) {
    return NULL;
  }

  r_obj* val = sparse_extract_val(x);
  const double* v_val = r_dbl_cbegin(val);

  const r_ssize n_pos = r_length(sparse_extract_pos(x));

  const r_ssize size = r_length(indx);

  r_ssize n_hits = 0;
  r_obj* matches = KEEP(sparse_subset_matches(x, indx, &n_hits));
  const int* v_matches = r_int_cbegin(matches);

  r_obj* out_val = KEEP(r_alloc_double(n_hits));
  double* v_out_val = r_dbl_begin(out_val);

  r_obj* out = KEEP(sparse_subset_list(x, size, out_val));
  int* v_out_pos = r_int_begin(r_list_get(out, 1));

  sparse_dbl_subset(v_val, v_matches, size, n_pos, v_out_val, v_out_pos);

  r_obj* altrep = new_altrep_sparse_double(out);

  FREE(3);
  return altrep;
}

// -----------------------------------------------------------------------------
// ALTREP

static r_ssize altrep_sparse_double_Length(r_obj* x) {
  const r_ssize out = sparse_extract_len(x);

  return out;
}

// What gets printed when .Internal(inspect()) is used
static Rboolean altrep_sparse_double_Inspect(
  r_obj* x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return sparse_inspect(x, "altrep_sparse_double");
}

static r_obj* altrep_sparse_double_Duplicate(r_obj* x, Rboolean deep) {
  return sparse_duplicate(x, deep, new_altrep_sparse_double);
}

// -----------------------------------------------------------------------------
// ALTREAL

static double altrep_sparse_double_Elt(r_obj* x, r_ssize i) {
  r_obj* data2 = R_altrep_data2(x);

  if (data2 != r_null) {
    return r_dbl_get(data2, i);
  }

  const r_ssize loc = sparse_elt_loc(x, i);

  if (loc < 0) {
    // OOB of vector itself
    return r_globals.na_dbl;
  }

  if (loc == r_length(sparse_extract_pos(x))) {
    // Can't find it, must be the default value
    return sparse_extract_default_double(x);
  } else {
    // Look it up in `val`
    return r_dbl_get(sparse_extract_val(x), loc);
  }
}

static int altrep_sparse_double_Is_sorted(r_obj* x) {
  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize pos_len = r_length(pos);

  r_obj* val = sparse_extract_val(x);
  const double* v_val = r_dbl_cbegin(val);

  const double v_default_val = sparse_extract_default_double(x);

  // zero length vector are by def sorted
  if (pos_len == 0) {
    return TRUE;
  }

  double current_value;

  if (v_pos[0] == 1) {
    current_value = v_val[0];
  } else {
    current_value = v_default_val;
  }

  for (r_ssize i = 0; i < pos_len; i++) {
    if (R_IsNA(v_val[i])) {
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

static r_obj* altrep_sparse_double_Min_method(r_obj* x, Rboolean na_rm) {
  if (sparse_extract_len(x) == 0) {
    Rf_warning("no non-missing arguments to min; returning Inf");
    return r_dbl(R_PosInf);
  }

  return altrep_sparse_double_extreme(x, na_rm, false);
}

static r_obj* altrep_sparse_double_Max_method(r_obj* x, Rboolean na_rm) {
  if (sparse_extract_len(x) == 0) {
    Rf_warning("no non-missing arguments to max; returning -Inf");
    return r_dbl(R_NegInf);
  }

  return altrep_sparse_double_extreme(x, na_rm, true);
}

static r_obj* altrep_sparse_double_extreme(
  r_obj* x,
  Rboolean na_rm,
  bool is_max
) {
  double out = is_max ? R_NegInf : R_PosInf;

  r_obj* val = sparse_extract_val(x);
  const double* v_val = r_dbl_cbegin(val);
  const r_ssize val_len = r_length(val);

  const double v_default_val = sparse_extract_default_double(x);

  // The default only appears if some positions are not explicitly stored
  if (
    val_len < sparse_extract_len(x) &&
    (is_max ? v_default_val > out : v_default_val < out)
  ) {
    out = v_default_val;
  }

  for (r_ssize i = 0; i < val_len; i++) {
    if (R_IsNA(v_val[i]) && !na_rm) {
      return r_dbl(r_globals.na_dbl);
    }

    if (is_max ? v_val[i] > out : v_val[i] < out) {
      out = v_val[i];
    }
  }
  return r_dbl(out);
}

static int altrep_sparse_double_No_NA_method(r_obj* x) {
  r_obj* val = sparse_extract_val(x);
  const double* v_val = r_dbl_cbegin(val);
  const r_ssize val_len = r_length(val);

  for (r_ssize i = 0; i < val_len; i++) {
    if (R_IsNA(v_val[i])) {
      return FALSE;
    }
  }

  return TRUE;
}

static r_obj* altrep_sparse_double_Sum_method(r_obj* x, Rboolean na_rm) {
  r_obj* val = sparse_extract_val(x);
  const double* v_val = r_dbl_cbegin(val);
  const r_ssize val_len = r_length(val);
  const r_ssize len = sparse_extract_len(x);

  double sum = 0;

  if (len == 0) {
    return r_dbl(sum);
  }

  for (r_ssize i = 0; i < val_len; i++) {
    if (R_IsNA(v_val[i])) {
      if (na_rm) {
        continue;
      } else {
        return r_dbl(r_globals.na_dbl);
      }
    }
    sum = sum + v_val[i];
  }

  // default can be non-zero
  const double v_default_val = sparse_extract_default_double(x);

  if (v_default_val != 0) {
    sum = sum + (len - val_len) * v_default_val;
  }

  return r_dbl(sum);
}
