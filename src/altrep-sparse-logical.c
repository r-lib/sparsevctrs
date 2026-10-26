#include "sparsevctrs-core.h"

#include "sparse-utils.h"
#include "altrep-sparse-logical.h"
#include "altrep-sparse-common.h"

#include "decl/altrep-sparse-logical-decl.h"

// Initialised at load time

R_altrep_class_t altrep_sparse_logical_class;

r_obj* ffi_sparse_altrep_new_sparse_logical(r_obj* ffi_x) {
  return new_altrep_sparse_logical(ffi_x);
}

r_obj* ffi_sparse_altrep_sparse_logical_materialize(r_obj* ffi_x) {
  return altrep_sparse_logical_Materialize(ffi_x);
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_logical(DllInfo* dll) {
  altrep_sparse_logical_class =
    R_make_altlogical_class("altrep_sparse_logical", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Dataptr
  );
  R_set_altvec_Dataptr_or_null_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Dataptr_or_null
  );
  R_set_altvec_Extract_subset_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Extract_subset
  );

  // ALTREP
  R_set_altrep_Length_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Length
  );
  R_set_altrep_Inspect_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Inspect
  );
  R_set_altrep_Duplicate_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Duplicate
  );

  // ALTLOGICAL
  R_set_altlogical_Elt_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Elt
  );
  R_set_altlogical_Is_sorted_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Is_sorted
  );
  R_set_altlogical_No_NA_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_No_NA_method
  );
  R_set_altlogical_Sum_method(
    altrep_sparse_logical_class,
    altrep_sparse_logical_Sum_method
  );
}

r_obj* new_altrep_sparse_logical(r_obj* data1) {
  return R_new_altrep(altrep_sparse_logical_class, data1, r_null);
}

static r_obj* altrep_sparse_logical_Materialize(r_obj* x) {
  r_obj* out = R_altrep_data2(x);

  if (out != r_null) {
    return out;
  }

  sparse_verbose_materialize();

  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_lgl_cbegin(val);

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize len = sparse_extract_len(x);

  const int v_default_val = sparse_extract_default_logical(x);

  out = KEEP(r_alloc_logical(len));
  int* v_out = r_lgl_begin(out);

  sparse_int_materialize(
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

static void* altrep_sparse_logical_Dataptr(r_obj* x, Rboolean writeable) {
  return DATAPTR_RW(altrep_sparse_logical_Materialize(x));
}

static const void* altrep_sparse_logical_Dataptr_or_null(r_obj* x) {
  return sparse_dataptr_or_null(x);
}

static r_obj* altrep_sparse_logical_Extract_subset(
  r_obj* x,
  r_obj* indx,
  r_obj* call
) {
  if (!sparse_is_index_handleable(indx)) {
    return NULL;
  }

  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_lgl_cbegin(val);

  const r_ssize n_pos = r_length(sparse_extract_pos(x));

  const r_ssize size = r_length(indx);

  r_ssize n_hits = 0;
  r_obj* matches = KEEP(sparse_subset_matches(x, indx, &n_hits));
  const int* v_matches = r_int_cbegin(matches);

  r_obj* out_val = KEEP(r_alloc_logical(n_hits));
  int* v_out_val = r_lgl_begin(out_val);

  r_obj* out = KEEP(sparse_subset_list(x, size, out_val));
  int* v_out_pos = r_int_begin(r_list_get(out, 1));

  sparse_int_subset(
    v_val,
    v_matches,
    size,
    n_pos,
    r_globals.na_lgl,
    v_out_val,
    v_out_pos
  );

  r_obj* altrep = new_altrep_sparse_logical(out);

  FREE(3);
  return altrep;
}

// -----------------------------------------------------------------------------
// ALTREP

static r_ssize altrep_sparse_logical_Length(r_obj* x) {
  const r_ssize out = sparse_extract_len(x);

  return out;
}

// What gets printed when .Internal(inspect()) is used
static Rboolean altrep_sparse_logical_Inspect(
  r_obj* x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return sparse_inspect(x, "altrep_sparse_logical");
}

static r_obj* altrep_sparse_logical_Duplicate(r_obj* x, Rboolean deep) {
  return sparse_duplicate(x, deep, new_altrep_sparse_logical);
}

// -----------------------------------------------------------------------------
// ALTLOGICAL

static int altrep_sparse_logical_Elt(r_obj* x, r_ssize i) {
  r_obj* data2 = R_altrep_data2(x);

  if (data2 != r_null) {
    return r_lgl_get(data2, i);
  }

  const r_ssize loc = sparse_elt_loc(x, i);

  if (loc < 0) {
    // OOB of vector itself
    return r_globals.na_lgl;
  }

  if (loc == r_length(sparse_extract_pos(x))) {
    // Can't find it, must be the default value
    return sparse_extract_default_logical(x);
  } else {
    // Look it up in `val`
    return r_lgl_get(sparse_extract_val(x), loc);
  }
}

static int altrep_sparse_logical_Is_sorted(r_obj* x) {
  const int* v_val = r_lgl_cbegin(sparse_extract_val(x));

  return sparse_int_is_sorted(x, v_val, sparse_extract_default_logical(x));
}

static int altrep_sparse_logical_No_NA_method(r_obj* x) {
  const int* v_val = r_lgl_cbegin(sparse_extract_val(x));

  return sparse_int_no_na(x, v_val);
}

static r_obj* altrep_sparse_logical_Sum_method(r_obj* x, Rboolean na_rm) {
  r_obj* val = sparse_extract_val(x);
  const int* v_val = r_lgl_cbegin(val);
  const r_ssize val_len = r_length(val);
  const r_ssize len = sparse_extract_len(x);

  int sum = 0;

  if (len == 0) {
    return r_lgl(sum);
  }

  for (r_ssize i = 0; i < val_len; i++) {
    if (v_val[i] == r_globals.na_lgl) {
      if (na_rm) {
        continue;
      } else {
        r_obj* out = r_alloc_logical(1);
        r_lgl_poke(out, 0, r_globals.na_lgl);
        return out;
      }
    }
    sum = sum + v_val[i];
  }

  // default can be non-zero
  const int v_default_val = sparse_extract_default_logical(x);

  if (v_default_val != 0) {
    sum = sum + (len - val_len) * v_default_val;
  }

  return r_lgl(sum);
}
