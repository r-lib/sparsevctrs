#define R_NO_REMAP
#include "R.h"
#include <Rinternals.h>
#include <R_ext/Altrep.h>
#include "altrep-sparse-utils.h"

// Initialised at load time
R_altrep_class_t altrep_sparse_real_class;

SEXP ffi_altrep_new_sparse_real(SEXP x) {
  return R_new_altrep(altrep_sparse_real_class, x, R_NilValue);
}

SEXP alrep_sparse_real_Materialize(SEXP x) {
  SEXP out = R_altrep_data2(x);

  if (out != R_NilValue) {
    return out;
  }

  SEXP val = extract_val(x);
  SEXP pos = extract_pos(x);
  R_xlen_t len = extract_len(x);

  out = PROTECT(Rf_allocVector(REALSXP, len));
  
  // Reminder about performance
  for (R_xlen_t i = 0; i < len; ++i) {
    SET_REAL_ELT(out, i, 0);
  }

  R_xlen_t n_positions = Rf_xlength(pos);

  // Reminder about performance
  for (R_xlen_t i = 0; i < n_positions; ++i) {
    SET_REAL_ELT(out, INTEGER_ELT(pos, i) - 1, REAL_ELT(val, i));
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
  SEXP val_old = extract_val(x);
  SEXP pos_old = extract_pos(x);
  SEXP matches = PROTECT(Rf_match(pos_old, indx, R_NaInt));

  int n = 0;

  for (int i = 0; i < Rf_length(matches); ++i) { 
    if (INTEGER_ELT(matches, i) != R_NaInt) {
      n++;
    }
  }

  SEXP val_new = PROTECT(Rf_allocVector(REALSXP, n));
  SEXP pos_new = PROTECT(Rf_allocVector(INTSXP, n));
 
  int step = 0;
  int what_pos = 1;

  for (int i = 0; i < Rf_length(matches); ++i) {

    int match = INTEGER_ELT(matches, i);
    if (match != R_NaInt) {
      SET_REAL_ELT(val_new, step, REAL_ELT(val_old, match - 1));

      for (int j = 0; j < Rf_length(matches); ++j) {
        if (INTEGER_ELT(indx, j) == INTEGER_ELT(pos_old, match - 1)) {
          break;
        } else {
          what_pos++;
        }
      }
      SET_INTEGER_ELT(pos_new, step, what_pos);
      what_pos = 1;
      step++;
    }
  }

  const char *names[] = {"val", "pos", "length", ""};
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, names)); 
  SET_VECTOR_ELT(res, 0, val_new);
  SET_VECTOR_ELT(res, 1, pos_new);
  SET_VECTOR_ELT(res, 2, Rf_ScalarInteger(Rf_length(matches)));

  UNPROTECT(4);

  return ffi_altrep_new_sparse_real(res);
}

// -----------------------------------------------------------------------------
// ALTREP

R_xlen_t altrep_sparse_real_Length(SEXP x) {
  R_xlen_t out = extract_len(x);

  return out;
}

// What gets printed when .Internal(inspect()) is used
Rboolean altrep_sparse_real_Inspect(SEXP x,
                                    int pre,
                                    int deep,
                                    int pvec,
                                    void (*inspect_subtree)(SEXP, int, int, int)) {
  Rprintf("sparsevctrs_altrep_sparse_real (materialized=%s)\n",
          R_altrep_data2(x) != R_NilValue ? "T" : "F");
  return TRUE;
}

// -----------------------------------------------------------------------------
// ALTREAL

static double altrep_sparse_real_Elt(SEXP x, R_xlen_t i) {
  SEXP val = extract_val(x);
  SEXP pos = extract_pos(x);
  R_xlen_t len = extract_len(x);

  if (i > len) {
    return NA_REAL;
  }
  
  const R_xlen_t n = Rf_xlength(val);

  double out = 0;

  for (int j = 0; j < n; ++j) {
    if (INTEGER_ELT(pos, j) == i + 1) {
      out = REAL_ELT(val, j);
      break;
    }
  }
  
  return out;
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_real(DllInfo* dll) {
  altrep_sparse_real_class =  R_make_altreal_class("altrep_sparse_real", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(altrep_sparse_real_class, altrep_sparse_real_Dataptr);
  R_set_altvec_Dataptr_or_null_method(altrep_sparse_real_class, altrep_sparse_real_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(altrep_sparse_real_class, altrep_sparse_real_Extract_subset);

  // ALTREP
  R_set_altrep_Length_method(altrep_sparse_real_class, altrep_sparse_real_Length);
  R_set_altrep_Inspect_method(altrep_sparse_real_class, altrep_sparse_real_Inspect);

  // ALTREAL
  R_set_altreal_Elt_method(altrep_sparse_real_class, altrep_sparse_real_Elt);
}
