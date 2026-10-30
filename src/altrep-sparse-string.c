#include "sparsevctrs-core.h"
#include <string.h>

#include "sparse-utils.h"
#include "altrep-sparse-string.h"
#include "altrep-sparse-common.h"

#include "decl/altrep-sparse-string-decl.h"

// Initialised at load time

R_altrep_class_t altrep_sparse_string_class;

r_obj* ffi_sparse_altrep_new_sparse_string(r_obj* ffi_x) {
  return new_altrep_sparse_string(ffi_x);
}

r_obj* ffi_sparse_altrep_sparse_string_materialize(r_obj* ffi_x) {
  return altrep_sparse_string_Materialize(ffi_x);
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_string(DllInfo* dll) {
  altrep_sparse_string_class =
    R_make_altstring_class("altrep_sparse_string", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Dataptr
  );
  R_set_altvec_Dataptr_or_null_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Dataptr_or_null
  );
  R_set_altvec_Extract_subset_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Extract_subset
  );

  // ALTREP
  R_set_altrep_Length_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Length
  );
  R_set_altrep_Inspect_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Inspect
  );
  R_set_altrep_Duplicate_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Duplicate
  );

  // ALTSTRING
  R_set_altstring_Elt_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Elt
  );
  R_set_altstring_Is_sorted_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Is_sorted
  );
  R_set_altstring_Set_elt_method(
    altrep_sparse_string_class,
    altrep_sparse_string_Set_elt
  );
}

r_obj* new_altrep_sparse_string(r_obj* data1) {
  return R_new_altrep(altrep_sparse_string_class, data1, r_null);
}

static r_obj* altrep_sparse_string_Materialize(r_obj* x) {
  r_obj* out = R_altrep_data2(x);

  if (out != r_null) {
    return out;
  }

  sparse_verbose_materialize();

  r_obj* val = sparse_extract_val(x);

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize len = sparse_extract_len(x);

  r_obj* v_default_val = sparse_extract_default_string(x);

  out = KEEP(r_alloc_character(len));

  sparse_chr_materialize(out, len, v_default_val, val, v_pos, r_length(pos));

  R_set_altrep_data2(x, out);
  // R_set_altrep_data1(x, R_NilValue);

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------
// ALTVEC

static void* altrep_sparse_string_Dataptr(r_obj* x, Rboolean writeable) {
  r_obj* out = altrep_sparse_string_Materialize(x);
  if (writeable) {
    sparse_mark_dirty(x);
  }
  return DATAPTR_RW(out);
}

static const void* altrep_sparse_string_Dataptr_or_null(r_obj* x) {
  return sparse_dataptr_or_null(x);
}

static r_obj* altrep_sparse_string_Extract_subset(
  r_obj* x,
  r_obj* indx,
  r_obj* call
) {
  if (!sparse_is_index_handleable(indx)) {
    return NULL;
  }

  r_obj* val = sparse_extract_val(x);

  const r_ssize n_pos = r_length(sparse_extract_pos(x));

  const r_ssize size = r_length(indx);

  r_ssize n_hits = 0;
  r_obj* matches = KEEP(sparse_subset_matches(x, indx, &n_hits));
  const int* v_matches = r_int_cbegin(matches);

  r_obj* out_val = KEEP(r_alloc_character(n_hits));

  r_obj* out = KEEP(sparse_subset_list(x, size, out_val));
  int* v_out_pos = r_int_begin(r_list_get(out, 1));

  sparse_chr_subset(val, v_matches, size, n_pos, out_val, v_out_pos);

  r_obj* altrep = new_altrep_sparse_string(out);

  FREE(3);
  return altrep;
}

// -----------------------------------------------------------------------------
// ALTREP

static r_ssize altrep_sparse_string_Length(r_obj* x) {
  const r_ssize out = sparse_extract_len(x);

  return out;
}

// What gets printed when .Internal(inspect()) is used
static Rboolean altrep_sparse_string_Inspect(
  r_obj* x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return sparse_inspect(x, "altrep_sparse_string");
}

static r_obj* altrep_sparse_string_Duplicate(r_obj* x, Rboolean deep) {
  return sparse_duplicate(x, deep, new_altrep_sparse_string);
}

// -----------------------------------------------------------------------------
// ALTSTRING

static r_obj* altrep_sparse_string_Elt(r_obj* x, r_ssize i) {
  r_obj* data2 = R_altrep_data2(x);

  if (data2 != r_null) {
    return r_chr_get(data2, i);
  }

  r_obj* val = sparse_extract_val(x);

  const r_ssize loc = sparse_elt_loc(x, i);

  if (loc < 0) {
    // OOB of vector itself
    return r_globals.na_str;
  }

  if (loc == r_length(sparse_extract_pos(x))) {
    // Can't find it, must be the default value
    return sparse_extract_default_string(x);
  } else {
    // Look it up in `val`
    return r_chr_get(val, loc);
  }
}

static int altrep_sparse_string_Is_sorted(r_obj* x) {
  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize pos_len = r_length(pos);

  r_obj* val = sparse_extract_val(x);

  r_obj* v_default_val = sparse_extract_default_string(x);

  // zero length vector are by def sorted
  if (pos_len == 0) {
    return TRUE;
  }

  r_obj* current_value;

  if (v_pos[0] == 1) {
    current_value = r_chr_get(val, 0);
  } else {
    current_value = v_default_val;
  }

  for (r_ssize i = 0; i < pos_len; i++) {
    if (r_chr_get(val, i) == r_globals.na_str) {
      return FALSE;
    }

    if (compare_strings(r_chr_get(val, i), current_value) < 0) {
      return FALSE;
    }

    current_value = r_chr_get(val, i);

    if (i + 1 == pos_len) {
      break;
    }

    // If there is a gap between values check against default
    if ((v_pos[i + 1] - v_pos[i]) > 1) {
      if (compare_strings(v_default_val, current_value) < 0) {
        return FALSE;
      }

      current_value = v_default_val;
    }
  }

  // Trailing default values after the last stored value
  if (v_pos[pos_len - 1] < sparse_extract_len(x)) {
    if (compare_strings(v_default_val, current_value) < 0) {
      return FALSE;
    }
  }

  return TRUE;
}

// Locale-aware comparison, matching what `is.unsorted()` does for strings
static int compare_strings(r_obj* a, r_obj* b) {
  return strcoll(Rf_translateChar(a), Rf_translateChar(b));
}

static void altrep_sparse_string_Set_elt(r_obj* x, r_ssize i, r_obj* value) {
  r_obj* out = R_altrep_data2(x);

  if (out == r_null) {
    out = altrep_sparse_string_Materialize(x);
  }

  sparse_mark_dirty(x);
  r_chr_poke(out, i, value);
}
