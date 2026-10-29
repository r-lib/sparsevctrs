#include "sparsevctrs-core.h"

// Defined in altrep-sparse-double.c
extern r_obj* ffi_sparse_altrep_new_sparse_double(r_obj*);
extern void sparsevctrs_init_altrep_sparse_double(DllInfo*);
extern r_obj* ffi_sparse_altrep_sparse_double_materialize(r_obj*);

// Defined in altrep-sparse-integer.c
extern r_obj* ffi_sparse_altrep_new_sparse_integer(r_obj*);
extern void sparsevctrs_init_altrep_sparse_integer(DllInfo*);
extern r_obj* ffi_sparse_altrep_sparse_integer_materialize(r_obj*);

// Defined in altrep-sparse-string.c
extern r_obj* ffi_sparse_altrep_new_sparse_string(r_obj*);
extern void sparsevctrs_init_altrep_sparse_string(DllInfo*);
extern r_obj* ffi_sparse_altrep_sparse_string_materialize(r_obj*);

// Defined in altrep-sparse-logical.c
extern r_obj* ffi_sparse_altrep_new_sparse_logical(r_obj*);
extern void sparsevctrs_init_altrep_sparse_logical(DllInfo*);
extern r_obj* ffi_sparse_altrep_sparse_logical_materialize(r_obj*);

// Defined in sparse-coerce.c
extern r_obj* ffi_sparse_matrix_to_list(r_obj*, r_obj*, r_obj*, r_obj*);

// Defined in sparse-extractors.c
extern r_obj* ffi_sparse_altrep_sparse_positions(r_obj*);
extern r_obj* ffi_sparse_altrep_sparse_values(r_obj*);
extern r_obj* ffi_sparse_altrep_sparse_default(r_obj*);

// Defined in sparse-utils.c
extern r_obj* ffi_sparse_extract_altrep_class(r_obj*);
extern r_obj* ffi_sparse_is_sparse_vector(r_obj*);
extern r_obj* ffi_sparse_is_altrep_non_sparse_vector(r_obj*);

// Defined in sparse-dummy.c
extern r_obj* ffi_sparse_dummy(r_obj*, r_obj*, r_obj*);

// Defined in sparse-arithmetic.c
extern r_obj* ffi_sparse_multiplication(r_obj*, r_obj*);

// Defined in sparse-arithmetic-scalar.c
extern r_obj* ffi_sparse_addition_scalar(r_obj*, r_obj*);
extern r_obj* ffi_sparse_subtraction_scalar(r_obj*, r_obj*);
extern r_obj* ffi_sparse_multiplication_scalar(r_obj*, r_obj*);
extern r_obj* ffi_sparse_division_scalar(r_obj*, r_obj*);

// Defined below
extern r_obj* ffi_init_library(r_obj*);

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
  {"ffi_sparse_dummy", (DL_FUNC) &ffi_sparse_dummy, 3},
  {"ffi_sparse_multiplication", (DL_FUNC) &ffi_sparse_multiplication, 2},
  {"ffi_sparse_addition_scalar", (DL_FUNC) &ffi_sparse_addition_scalar, 2},
  {"ffi_sparse_subtraction_scalar",
   (DL_FUNC) &ffi_sparse_subtraction_scalar,
   2},
  {"ffi_sparse_multiplication_scalar",
   (DL_FUNC) &ffi_sparse_multiplication_scalar,
   2},
  {"ffi_sparse_division_scalar", (DL_FUNC) &ffi_sparse_division_scalar, 2},
  {NULL, NULL, 0}
};

r_obj* ffi_init_library(r_obj* ffi_ns) {
  r_init_library(ffi_ns);
  return r_null;
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
