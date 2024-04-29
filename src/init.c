#include <Rinternals.h>

// Defined in altrep-sparse-real.c
extern SEXP ffi_altrep_new_sparse_real(SEXP);
extern void sparsevctrs_init_altrep_sparse_real(DllInfo*);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_altrep_new_sparse_real", (DL_FUNC) &ffi_altrep_new_sparse_real, 1},
    {NULL, NULL, 0}
};

void R_init_sparsevctrs(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // altrep classes
  sparsevctrs_init_altrep_sparse_real(dll);
}
