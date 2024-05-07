#define R_NO_REMAP
#include <Rinternals.h>
#include <R_ext/Altrep.h>

#include "sparse-utils.h"

// Initialised at load time
R_altrep_class_t altrep_sparse_real_class;

SEXP ffi_altrep_new_sparse_double(SEXP x) {
  return R_new_altrep(altrep_sparse_real_class, x, R_NilValue);
}

SEXP alrep_sparse_real_Materialize(SEXP x) {
  if (!Rf_isNull(Rf_GetOption1(Rf_install("sparsevctrs.verbose_materialize"))
      )) {
    Rprintf("sparsevctrs: Sparse vector materialized\n");
  }

  SEXP out = R_altrep_data2(x);

  if (out != R_NilValue) {
    return out;
  }

  SEXP val = extract_val(x);
  const double* v_val = REAL_RO(val);

  SEXP pos = extract_pos(x);
  const int* v_pos = INTEGER_RO(pos);

  const R_xlen_t len = extract_len(x);

  out = PROTECT(Rf_allocVector(REALSXP, len));
  double* v_out = REAL(out);

  for (R_xlen_t i = 0; i < len; ++i) {
    v_out[i] = 0;
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

void* altrep_sparse_real_Dataptr(SEXP x, Rboolean writeable) {
  return STDVEC_DATAPTR(alrep_sparse_real_Materialize(x));
}

const void* altrep_sparse_real_Dataptr_or_null(SEXP x) {
  SEXP out = R_altrep_data2(x);

  if (out == R_NilValue) {
    return NULL;
  } else {
    return out;
  }
}

static SEXP altrep_sparse_real_Extract_subset(SEXP x, SEXP indx, SEXP call) {
  if (!is_index_handleable(indx)) {
    return NULL;
  }

  const R_xlen_t len = extract_len(x);

  SEXP val = extract_val(x);
  const double* v_val = REAL_RO(val);

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

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));

  SEXP out_val = Rf_allocVector(REALSXP, n_hits);
  SET_VECTOR_ELT(out, 0, out_val);
  double* v_out_val = REAL(out_val);

  SEXP out_pos = Rf_allocVector(INTSXP, n_hits);
  SET_VECTOR_ELT(out, 1, out_pos);
  int* v_out_pos = INTEGER(out_pos);

  SEXP out_length = Rf_ScalarInteger((int) size);
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP names = Rf_allocVector(STRSXP, 3);
  Rf_setAttrib(out, R_NamesSymbol, names);
  SET_STRING_ELT(names, 0, Rf_mkChar("val"));
  SET_STRING_ELT(names, 1, Rf_mkChar("pos"));
  SET_STRING_ELT(names, 2, Rf_mkChar("len"));

  R_xlen_t i_out = 0;

  for (R_xlen_t i = 0; i < size; ++i) {
    const int match = v_matches[i];

    if (match == (int) n_pos) {
      // Default value case
      continue;
    }

    if (match == NA_INTEGER) {
      v_out_val[i_out] = NA_REAL;
      v_out_pos[i_out] = (int) i + 1;
      ++i_out;
      continue;
    }

    // Otherwise we have a hit from `pos`
    v_out_val[i_out] = v_val[match];
    v_out_pos[i_out] = (int) i + 1;
    ++i_out;
  }

  SEXP altrep = ffi_altrep_new_sparse_double(out);

  UNPROTECT(2);
  return altrep;
}

// -----------------------------------------------------------------------------
// ALTREP

R_xlen_t altrep_sparse_real_Length(SEXP x) {
  R_xlen_t out = extract_len(x);

  return out;
}

// What gets printed when .Internal(inspect()) is used
Rboolean altrep_sparse_real_Inspect(
    SEXP x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(SEXP, int, int, int)
) {
  Rprintf(
      "sparsevctrs_altrep_sparse_real (materialized=%s, length=%i)\n",
      R_altrep_data2(x) != R_NilValue ? "T" : "F",
      (int) extract_len(x)
  );
  return TRUE;
}

// -----------------------------------------------------------------------------
// ALTREAL

static double altrep_sparse_real_Elt(SEXP x, R_xlen_t i) {
  SEXP val = extract_val(x);

  SEXP pos = extract_pos(x);
  const int* v_pos = INTEGER_RO(pos);
  const R_xlen_t size = Rf_xlength(pos);

  const R_xlen_t len = extract_len(x);

  if (i > len) {
    // OOB of vector itself
    return NA_REAL;
  }

  // TODO: Add `r_xlen_t_to_int()`
  const int needle = (int) i + 1;
  const R_xlen_t loc = binary_search(needle, v_pos, size);

  if (loc == size) {
    // Can't find it, must be the default value
    return 0;
  } else {
    // Look it up in `val`
    return REAL_ELT(val, loc);
  }
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_real(DllInfo* dll) {
  altrep_sparse_real_class =
      R_make_altreal_class("altrep_sparse_real", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(
      altrep_sparse_real_class, altrep_sparse_real_Dataptr
  );
  R_set_altvec_Dataptr_or_null_method(
      altrep_sparse_real_class, altrep_sparse_real_Dataptr_or_null
  );
  R_set_altvec_Extract_subset_method(
      altrep_sparse_real_class, altrep_sparse_real_Extract_subset
  );

  // ALTREP
  R_set_altrep_Length_method(
      altrep_sparse_real_class, altrep_sparse_real_Length
  );
  R_set_altrep_Inspect_method(
      altrep_sparse_real_class, altrep_sparse_real_Inspect
  );

  // ALTREAL
  R_set_altreal_Elt_method(altrep_sparse_real_class, altrep_sparse_real_Elt);
}
