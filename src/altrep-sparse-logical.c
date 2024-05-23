#define R_NO_REMAP
#include <Rinternals.h>
#include <R_ext/Altrep.h>

#include "sparse-utils.h"

// Initialised at load time

R_altrep_class_t altrep_sparse_logical_class;

SEXP ffi_altrep_new_sparse_logical(SEXP x) {
  return R_new_altrep(altrep_sparse_logical_class, x, R_NilValue);
}

SEXP alrep_sparse_logical_Materialize(SEXP x) {
  if (!Rf_isNull(Rf_GetOption1(Rf_install("sparsevctrs.verbose_materialize"))
      )) {
    Rprintf("sparsevctrs: Sparse vector materialized\n");
  }

  SEXP out = R_altrep_data2(x);

  if (out != R_NilValue) {
    return out;
  }

  SEXP val = extract_val(x);
  const int* v_val = LOGICAL_RO(val);

  SEXP pos = extract_pos(x);
  const int* v_pos = INTEGER_RO(pos);

  const R_xlen_t len = extract_len(x);

  const int v_default_val = extract_default_logical(x);

  out = PROTECT(Rf_allocVector(LGLSXP, len));
  int* v_out = LOGICAL(out);

  for (R_xlen_t i = 0; i < len; ++i) {
    v_out[i] = v_default_val;
  }

  const R_xlen_t n_positions = Rf_xlength(pos);

  for (R_xlen_t i = 0; i < n_positions; ++i) {
    const int loc = v_pos[i] - 1;
    v_out[loc] = v_val[i];
  }

  R_set_altrep_data2(x, out);

  UNPROTECT(1);
  return out;
}

// -----------------------------------------------------------------------------
// ALTVEC

void* altrep_sparse_logical_Dataptr(SEXP x, Rboolean writeable) {
  return DATAPTR(alrep_sparse_logical_Materialize(x));
}

const void* altrep_sparse_logical_Dataptr_or_null(SEXP x) {
  SEXP out = R_altrep_data2(x);

  if (out == R_NilValue) {
    return NULL;
  } else {
    return DATAPTR(out);
  }
}

static SEXP altrep_sparse_logical_Extract_subset(SEXP x, SEXP indx, SEXP call) {
  if (!is_index_handleable(indx)) {
    return NULL;
  }

  const R_xlen_t len = extract_len(x);

  SEXP val = extract_val(x);
  const int* v_val = LOGICAL_RO(val);

  SEXP pos = extract_pos(x);
  const int* v_pos = INTEGER_RO(pos);
  const R_xlen_t n_pos = Rf_xlength(pos);

  const int* v_indx = INTEGER_RO(indx);

  const R_xlen_t size = Rf_xlength(indx);

  R_xlen_t n_hits = 0;
  SEXP matches = PROTECT(Rf_allocVector(INTSXP, size));
  int* v_matches = INTEGER(matches);

  for (R_xlen_t i = 0; i < size; ++i) {
    // 1 indexed!
    const int index = v_indx[i];

    if (index == NA_INTEGER) {
      v_matches[i] = NA_INTEGER;
      ++n_hits;
      continue;
    }

    if (index > len) {
      // (Uses `>` not `>=` because `index` is 1 indexed)
      // OOB
      v_matches[i] = NA_INTEGER;
      ++n_hits;
      continue;
    }

    const R_xlen_t loc = binary_search(index, v_pos, n_pos);

    if (loc == n_pos) {
      // Not in `pos`, gets default value
      v_matches[i] = (int) n_pos;
      continue;
    }

    // Did find in `pos`
    v_matches[i] = (int) loc;
    ++n_hits;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  SEXP out_val = Rf_allocVector(LGLSXP, n_hits);
  SET_VECTOR_ELT(out, 0, out_val);
  int* v_out_val = LOGICAL(out_val);

  SEXP out_pos = Rf_allocVector(INTSXP, n_hits);
  SET_VECTOR_ELT(out, 1, out_pos);
  int* v_out_pos = INTEGER(out_pos);

  SEXP out_length = Rf_ScalarInteger((int) size);
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP out_default = extract_default(x);
  SET_VECTOR_ELT(out, 3, out_default);

  SEXP names = Rf_allocVector(STRSXP, 4);
  Rf_setAttrib(out, R_NamesSymbol, names);
  SET_STRING_ELT(names, 0, Rf_mkChar("val"));
  SET_STRING_ELT(names, 1, Rf_mkChar("pos"));
  SET_STRING_ELT(names, 2, Rf_mkChar("len"));
  SET_STRING_ELT(names, 3, Rf_mkChar("default"));

  R_xlen_t i_out = 0;

  for (R_xlen_t i = 0; i < size; ++i) {
    const int match = v_matches[i];

    if (match == (int) n_pos) {
      // Default value case
      continue;
    }

    if (match == NA_INTEGER) {
      v_out_val[i_out] = NA_LOGICAL;
      v_out_pos[i_out] = (int) i + 1;
      ++i_out;
      continue;
    }

    // Otherwise we have a hit from `pos`
    v_out_val[i_out] = v_val[match];
    v_out_pos[i_out] = (int) i + 1;
    ++i_out;
  }

  SEXP altrep = ffi_altrep_new_sparse_logical(out);

  UNPROTECT(2);
  return altrep;
}

// -----------------------------------------------------------------------------
// ALTREP

R_xlen_t altrep_sparse_logical_Length(SEXP x) {
  R_xlen_t out = extract_len(x);

  return out;
}

// What gets printed when .Internal(inspect()) is used
Rboolean altrep_sparse_logical_Inspect(
    SEXP x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(SEXP, int, int, int)
) {
  Rprintf(
      "sparsevctrs_altrep_sparse_logical (materialized=%s, length=%i)\n",
      R_altrep_data2(x) != R_NilValue ? "T" : "F",
      (int) extract_len(x)
  );
  return TRUE;
}

SEXP altrep_sparse_logical_Duplicate(SEXP x, Rboolean deep) {
  SEXP data1 = R_altrep_data1(x);
  SEXP data2 = R_altrep_data2(x);

  /* If deep or already materialized, do the default behavior */
  if (deep || data2 != R_NilValue) {
    return NULL;
  }

  return ffi_altrep_new_sparse_logical(data1);
}

// -----------------------------------------------------------------------------
// ALTLOGICAL

static int altrep_sparse_logical_Elt(SEXP x, R_xlen_t i) {
  SEXP val = extract_val(x);

  SEXP pos = extract_pos(x);
  const int* v_pos = INTEGER_RO(pos);
  const R_xlen_t size = Rf_xlength(pos);

  const R_xlen_t len = extract_len(x);

  const int v_default_val = extract_default_logical(x);

  if (i > len) {
    // OOB of vector itself
    return NA_LOGICAL;
  }

  // TODO: Add `r_xlen_t_to_int()`
  const int needle = (int) i + 1;
  const R_xlen_t loc = binary_search(needle, v_pos, size);

  if (loc == size) {
    // Can't find it, must be the default value
    return v_default_val;
  } else {
    // Look it up in `val`
    return LOGICAL_ELT(val, loc);
  }
}

int altrep_sparse_logical_Is_sorted(SEXP x) {
  SEXP pos = extract_pos(x);
  const int* v_pos = INTEGER_RO(pos);

  const R_xlen_t pos_len = Rf_xlength(pos);

  SEXP val = extract_val(x);
  const int* v_val = LOGICAL_RO(val);

  const int v_default_val = extract_default_logical(x);

  // zero length vector are by def sorted
  if (pos_len == 0) {
    return TRUE;
  }

  // 1 length vector are by def sorted
  if (pos_len == 1) {
    if (R_IsNA(v_val[0])) {
      // unless equal to NA
      return FALSE;
    } else {
      return TRUE;
    }
  }

  int current_value;

  if (v_pos[0] == 1) {
    current_value = v_val[0];
  } else {
    current_value = v_default_val;
  }

  for (R_xlen_t i = 0; i < pos_len; i++) {
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

  return TRUE;
}

static int altrep_sparse_logical_No_NA_method(SEXP x) {
  const SEXP val = extract_val(x);
  const int* v_val = LOGICAL_RO(val);
  const R_xlen_t val_len = Rf_xlength(val);

  for (R_xlen_t i = 0; i < val_len; i++) {
    if (R_IsNA(v_val[i])) {
      return FALSE;
    }
  }

  return TRUE;
}

static SEXP altrep_sparse_logical_Sum_method(SEXP x, Rboolean na_rm) {
  const SEXP val = extract_val(x);
  const int* v_val = LOGICAL_RO(val);
  const R_xlen_t val_len = Rf_xlength(val);
  const R_xlen_t len = extract_len(x);

  int sum = 0;

  if (len == 0) {
    return Rf_ScalarLogical(sum);
  }

  for (R_xlen_t i = 0; i < val_len; i++) {
    if (R_IsNA(v_val[i])) {
      if (na_rm) {
        continue;
      } else {
        return Rf_ScalarLogical(NA_LOGICAL);
      }
    }
    sum = sum + v_val[i];
  }

  // default can be non-zero
  const int v_default_val = extract_default_integer(x);

  if (v_default_val != 0) {
    sum = sum + (len - val_len) * v_default_val;
  }

  return Rf_ScalarLogical(sum);
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_logical(DllInfo* dll) {
  altrep_sparse_logical_class =
      R_make_altlogical_class("altrep_sparse_logical", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Dataptr
  );
  R_set_altvec_Dataptr_or_null_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Dataptr_or_null
  );
  R_set_altvec_Extract_subset_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Extract_subset
  );

  // ALTREP
  R_set_altrep_Length_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Length
  );
  R_set_altrep_Inspect_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Inspect
  );
  R_set_altrep_Duplicate_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Duplicate
  );

  // ALTLOGICAL
  R_set_altlogical_Elt_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Elt
  );
  R_set_altlogical_Is_sorted_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Is_sorted
  );
  R_set_altlogical_No_NA_method(
      altrep_sparse_logical_class, altrep_sparse_logical_No_NA_method
  );
  R_set_altlogical_Sum_method(
      altrep_sparse_logical_class, altrep_sparse_logical_Sum_method
  );
}
