#define R_NO_REMAP
#include "R.h"
#include <Rinternals.h>
#include <R_ext/Altrep.h>

// Initialised at load time
R_altrep_class_t sparsevctrs_real_class;

SEXP ffi_altrep_new_sparse_real(SEXP sWhat) {
  return R_new_altrep(sparsevctrs_real_class, sWhat, R_NilValue);
}

// -----------------------------------------------------------------------------
// ALTVEC

static void* sparsevctrs_Dataptr(SEXP sx, Rboolean writeable) {
  return sx;
}

static const void* sparsevctrs_Dataptr_or_null(SEXP sx) {
  return sx;
}

// -----------------------------------------------------------------------------
// ALTREP

static R_xlen_t sparsevctrs_Length(SEXP sx) {
  double res = 32;
  return res;
}

// TODO add sparsevctrs_Inspect

// -----------------------------------------------------------------------------
// ALTREAL

static double sparsevctrs_real_Elt(SEXP sx, R_xlen_t i) {
  return i;
}

static R_xlen_t
sparsevctrs_real_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double* buf) {
  return n;
}

// -----------------------------------------------------------------------------

void InitRealClass(DllInfo* dll) {
  sparsevctrs_real_class =  R_make_altreal_class("sparsevctrs_real", "sparsevctrs", dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(sparsevctrs_real_class, sparsevctrs_Dataptr);
  R_set_altvec_Dataptr_or_null_method(sparsevctrs_real_class, sparsevctrs_Dataptr_or_null);

  // ALTREP
  R_set_altrep_Length_method(sparsevctrs_real_class, sparsevctrs_Length);
  // TODO add sparsevctrs_Inspect

  // ALTREAL
  R_set_altreal_Elt_method(sparsevctrs_real_class, sparsevctrs_real_Elt);
  R_set_altreal_Get_region_method(sparsevctrs_real_class, sparsevctrs_real_Get_region);
}
