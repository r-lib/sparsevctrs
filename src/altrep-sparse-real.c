#define R_NO_REMAP
#include "R.h"
#include <Rinternals.h>
#include <R_ext/Altrep.h>

// Initialised at load time
R_altrep_class_t altrep_sparse_real_class;

SEXP ffi_altrep_new_sparse_real(SEXP x) {
  return R_new_altrep(altrep_sparse_real_class, x, R_NilValue);
}

SEXP alrep_sparse_real_Materialize(SEXP vec) {
  SEXP out = R_altrep_data2(vec);

  if (out != R_NilValue) {
    return out;
  }

  SEXP data1 = PROTECT(R_altrep_data1(vec));
  SEXP val = PROTECT(VECTOR_ELT(data1, 0));
  SEXP pos = PROTECT(VECTOR_ELT(data1, 1));
  SEXP len = PROTECT(VECTOR_ELT(data1, 2));

  out = PROTECT(Rf_allocVector(REALSXP, Rf_asReal(len)));
  
  int n = Rf_asReal(len);
  
  for (int i = 0; i < n; ++i) {
    REAL(out)[i] = 0;
  }

  int m = Rf_length(pos);

  for (int i = 0; i < m; ++i) {
    REAL(out)[INTEGER(pos)[i]] = REAL(val)[i];
  }

  R_set_altrep_data2(vec, out);

  UNPROTECT(5);
  return out;
}

// -----------------------------------------------------------------------------
// ALTVEC

void* altrep_sparse_real_Dataptr(SEXP x, Rboolean writeable) {
  return STDVEC_DATAPTR(alrep_sparse_real_Materialize(x));
}

const void* altrep_sparse_real_Dataptr_or_null(SEXP vec) {
    SEXP out = R_altrep_data2(vec);

  if (out == R_NilValue) {
    return NULL;
  } else {
    return out;
  }
}

// -----------------------------------------------------------------------------
// ALTREP

R_xlen_t altrep_sparse_real_Length(SEXP x) {
  double out = Rf_asReal(VECTOR_ELT(R_altrep_data1(x),2));

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

  if (i > Rf_asReal(VECTOR_ELT(R_altrep_data1(x), 2))) {
    return NA_REAL;
  }

  SEXP data1 = PROTECT(R_altrep_data1(x));
  SEXP val = PROTECT(VECTOR_ELT(data1, 0));
  SEXP pos = PROTECT(VECTOR_ELT(data1, 1));
  
  const R_len_t n = Rf_length(val);

  double out = 0;

  for (int j = 0; j < n; ++j) {
    if (INTEGER(pos)[j] == i + 1) {
      out = REAL(val)[j];
      break;
    }
  }
  
  UNPROTECT(3);

  return out;
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_real(DllInfo* dll) {
  altrep_sparse_real_class =  R_make_altreal_class("altrep_sparse_real", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(altrep_sparse_real_class, altrep_sparse_real_Dataptr);
  R_set_altvec_Dataptr_or_null_method(altrep_sparse_real_class, altrep_sparse_real_Dataptr_or_null);

  // ALTREP
  R_set_altrep_Length_method(altrep_sparse_real_class, altrep_sparse_real_Length);
  R_set_altrep_Inspect_method(altrep_sparse_real_class, altrep_sparse_real_Inspect);

  // ALTREAL
  R_set_altreal_Elt_method(altrep_sparse_real_class, altrep_sparse_real_Elt);
}
