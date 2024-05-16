#include <Rinternals.h>
#include "sparse-extractors.h"
#include "sparse-utils.h"

// Defined in altrep-sparse-double.c
extern SEXP ffi_altrep_new_sparse_double(SEXP);
extern void sparsevctrs_init_altrep_sparse_double(DllInfo*);

// Defined in altrep-sparse-integer.c
extern SEXP ffi_altrep_new_sparse_integer(SEXP);
extern void sparsevctrs_init_altrep_sparse_integer(DllInfo*);

// Defined in altrep-sparse-string.c
extern SEXP ffi_altrep_new_sparse_string(SEXP);
extern void sparsevctrs_init_altrep_sparse_string(DllInfo*);

// Defined in altrep-sparse-logical.c
extern SEXP ffi_altrep_new_sparse_logical(SEXP);
extern void sparsevctrs_init_altrep_sparse_logical(DllInfo*);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_altrep_new_sparse_double",
     (DL_FUNC) &ffi_altrep_new_sparse_double,
     1},
    {"ffi_altrep_new_sparse_integer",
     (DL_FUNC) &ffi_altrep_new_sparse_integer,
     1},
    {"ffi_altrep_new_sparse_string",
     (DL_FUNC) &ffi_altrep_new_sparse_string,
     1},
    {"ffi_altrep_new_sparse_logical",
     (DL_FUNC) &ffi_altrep_new_sparse_logical,
     1},
    {"ffi_altrep_sparse_positions", (DL_FUNC) &ffi_altrep_sparse_positions, 1},
    {"ffi_altrep_sparse_values", (DL_FUNC) &ffi_altrep_sparse_values, 1},
    {"ffi_altrep_sparse_default", (DL_FUNC) &ffi_altrep_sparse_default, 1},
    {"ffi_extract_altrep_class", (DL_FUNC) &ffi_extract_altrep_class, 1},
    {NULL, NULL, 0}};

void R_init_sparsevctrs(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // altrep classes
  sparsevctrs_init_altrep_sparse_double(dll);
  sparsevctrs_init_altrep_sparse_integer(dll);
  sparsevctrs_init_altrep_sparse_string(dll);
  sparsevctrs_init_altrep_sparse_logical(dll);
}
