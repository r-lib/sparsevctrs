#define R_NO_REMAP
#include "R.h"
#include <Rinternals.h>
#include <R_ext/Altrep.h>

// Initialised at load time
R_altrep_class_t altrep_sparse_real_class;

SEXP ffi_altrep_new_sparse_real(SEXP sWhat, SEXP len) {
  return R_new_altrep(altrep_sparse_real_class, sWhat, len);
}

// -----------------------------------------------------------------------------
// ALTVEC

static void* altrep_sparse_real_Dataptr(SEXP sx, Rboolean writeable) {
  return sx;
}

static const void* altrep_sparse_real_Dataptr_or_null(SEXP sx) {
  return sx;
}

// -----------------------------------------------------------------------------
// ALTREP

R_xlen_t altrep_sparse_real_Length(SEXP x) {
  double out = REAL(R_altrep_data2(x))[0];

  return out;
}

// TODO add altrep_sparse_real_Inspect

// -----------------------------------------------------------------------------
// ALTREAL

static double altrep_sparse_real_real_Elt(SEXP sx, R_xlen_t i) {
  return i;
}

static R_xlen_t
altrep_sparse_real_real_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double* buf) {
  return n;
}

// -----------------------------------------------------------------------------

void sparsevctrs_init_altrep_sparse_real(DllInfo* dll) {
  altrep_sparse_real_class =  R_make_altreal_class("altrep_sparse_real", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(altrep_sparse_real_class, altrep_sparse_real_Dataptr);
  R_set_altvec_Dataptr_or_null_method(altrep_sparse_real_class, altrep_sparse_real_Dataptr_or_null);

  // ALTREP
  R_set_altrep_Length_method(altrep_sparse_real_class, altrep_sparse_real_Length);
  // TODO add altrep_sparse_real_Inspect

  // ALTREAL
  R_set_altreal_Elt_method(altrep_sparse_real_class, altrep_sparse_real_real_Elt);
  R_set_altreal_Get_region_method(altrep_sparse_real_class, altrep_sparse_real_real_Get_region);
}
