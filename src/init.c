#include <Rinternals.h>

// Defined in altrep-sparse-vector.c
extern SEXP do_sparsevctrs(SEXP);
extern void InitRealClass(DllInfo*);

static const R_CallMethodDef CallEntries[] = {
    {"sparsevctrs", (DL_FUNC) &do_sparsevctrs, 1},
    {NULL, NULL, 0}
};

void R_init_sparsevctrs(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // altrep classes
  InitRealClass(dll);
}
