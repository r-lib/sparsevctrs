#include <Rinternals.h>

// Defined in altrep-sparse-vector.c
extern SEXP ffi_altrep_new_sparse_real(SEXP);
extern void InitRealClass(DllInfo*);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_altrep_new_sparse_real", (DL_FUNC) &ffi_altrep_new_sparse_real, 1},
    {NULL, NULL, 0}
};

void R_init_sparsevctrs(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // altrep classes
  InitRealClass(dll);
}
