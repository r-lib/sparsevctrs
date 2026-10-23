#include "sparsevctrs-core.h"

// Defined in altrep-sparse-double.c
extern SEXP ffi_sparse_altrep_new_sparse_double(SEXP);
extern void sparsevctrs_init_altrep_sparse_double(DllInfo*);
extern SEXP ffi_sparse_altrep_sparse_double_materialize(SEXP);

// Defined in altrep-sparse-integer.c
extern SEXP ffi_sparse_altrep_new_sparse_integer(SEXP);
extern void sparsevctrs_init_altrep_sparse_integer(DllInfo*);
extern SEXP ffi_sparse_altrep_sparse_integer_materialize(SEXP);

// Defined in altrep-sparse-string.c
extern SEXP ffi_sparse_altrep_new_sparse_string(SEXP);
extern void sparsevctrs_init_altrep_sparse_string(DllInfo*);
extern SEXP ffi_sparse_altrep_sparse_string_materialize(SEXP);

// Defined in altrep-sparse-logical.c
extern SEXP ffi_sparse_altrep_new_sparse_logical(SEXP);
extern void sparsevctrs_init_altrep_sparse_logical(DllInfo*);
extern SEXP ffi_sparse_altrep_sparse_logical_materialize(SEXP);

// Defined in sparse-coerce.c
extern SEXP ffi_sparse_matrix_to_list(SEXP, SEXP, SEXP, SEXP);

// Defined in sparse-extractors.c
extern SEXP ffi_sparse_altrep_sparse_positions(SEXP);
extern SEXP ffi_sparse_altrep_sparse_values(SEXP);
extern SEXP ffi_sparse_altrep_sparse_default(SEXP);

// Defined in sparse-utils.c
extern SEXP ffi_sparse_extract_altrep_class(SEXP);
extern SEXP ffi_sparse_is_sparse_vector(SEXP);
extern SEXP ffi_sparse_is_altrep_non_sparse_vector(SEXP);

// Defined in sparse-dummy.c
extern SEXP ffi_sparse_dummy(SEXP, SEXP, SEXP, SEXP);
extern SEXP ffi_sparse_dummy_na(SEXP, SEXP, SEXP, SEXP);

// Defined in sparse-arithmatic.c
extern SEXP ffi_sparse_multiplication(SEXP, SEXP);

// Defined below
extern SEXP ffi_init_library(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"ffi_init_library", (DL_FUNC) &ffi_init_library, 1},
  {"ffi_sparse_matrix_to_list", (DL_FUNC) &ffi_sparse_matrix_to_list, 4},
  {"ffi_sparse_altrep_new_sparse_double",
   (DL_FUNC) &ffi_sparse_altrep_new_sparse_double,
   1},
  {"ffi_sparse_altrep_new_sparse_integer",
   (DL_FUNC) &ffi_sparse_altrep_new_sparse_integer,
   1},
  {"ffi_sparse_altrep_new_sparse_string",
   (DL_FUNC) &ffi_sparse_altrep_new_sparse_string,
   1},
  {"ffi_sparse_altrep_new_sparse_logical",
   (DL_FUNC) &ffi_sparse_altrep_new_sparse_logical,
   1},
  {"ffi_sparse_altrep_sparse_double_materialize",
   (DL_FUNC) &ffi_sparse_altrep_sparse_double_materialize,
   1},
  {"ffi_sparse_altrep_sparse_integer_materialize",
   (DL_FUNC) &ffi_sparse_altrep_sparse_integer_materialize,
   1},
  {"ffi_sparse_altrep_sparse_string_materialize",
   (DL_FUNC) &ffi_sparse_altrep_sparse_string_materialize,
   1},
  {"ffi_sparse_altrep_sparse_logical_materialize",
   (DL_FUNC) &ffi_sparse_altrep_sparse_logical_materialize,
   1},
  {"ffi_sparse_altrep_sparse_positions",
   (DL_FUNC) &ffi_sparse_altrep_sparse_positions,
   1},
  {"ffi_sparse_altrep_sparse_values",
   (DL_FUNC) &ffi_sparse_altrep_sparse_values,
   1},
  {"ffi_sparse_altrep_sparse_default",
   (DL_FUNC) &ffi_sparse_altrep_sparse_default,
   1},
  {"ffi_sparse_extract_altrep_class",
   (DL_FUNC) &ffi_sparse_extract_altrep_class,
   1},
  {"ffi_sparse_is_sparse_vector", (DL_FUNC) &ffi_sparse_is_sparse_vector, 1},
  {"ffi_sparse_is_altrep_non_sparse_vector",
   (DL_FUNC) &ffi_sparse_is_altrep_non_sparse_vector,
   1},
  {"ffi_sparse_dummy", (DL_FUNC) &ffi_sparse_dummy, 4},
  {"ffi_sparse_dummy_na", (DL_FUNC) &ffi_sparse_dummy_na, 4},
  {"ffi_sparse_multiplication", (DL_FUNC) &ffi_sparse_multiplication, 2},
  {NULL, NULL, 0}
};

SEXP ffi_init_library(SEXP ns) {
  r_init_library(ns);
  return R_NilValue;
}

attribute_visible void R_init_sparsevctrs(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  // altrep classes
  sparsevctrs_init_altrep_sparse_double(dll);
  sparsevctrs_init_altrep_sparse_integer(dll);
  sparsevctrs_init_altrep_sparse_string(dll);
  sparsevctrs_init_altrep_sparse_logical(dll);
}
