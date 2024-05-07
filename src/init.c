#include <Rinternals.h>
#include "sparse-extractors.h"
#include "sparse-utils.h"

// Defined in altrep-sparse-real.c
extern SEXP ffi_altrep_new_sparse_double(SEXP);
extern void sparsevctrs_init_altrep_sparse_double(DllInfo*);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_altrep_new_sparse_double",
     (DL_FUNC) &ffi_altrep_new_sparse_double,
     1},
    {"ffi_altrep_sparse_positions", (DL_FUNC) &ffi_altrep_sparse_positions, 1},
    {"ffi_altrep_sparse_values", (DL_FUNC) &ffi_altrep_sparse_values, 1},
    {"ffi_extract_altrep_class", (DL_FUNC) &ffi_extract_altrep_class, 1},
    {NULL, NULL, 0}};

void R_init_sparsevctrs(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // altrep classes
  sparsevctrs_init_altrep_sparse_double(dll);
}
