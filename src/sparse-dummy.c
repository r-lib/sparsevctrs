#include "sparse-dummy.h"

// Defined in altrep-sparse-integer.c
extern SEXP ffi_altrep_new_sparse_integer(SEXP);
extern void sparsevctrs_init_altrep_sparse_integer(DllInfo*);

SEXP ffi_sparse_dummy(SEXP x, SEXP lvls, SEXP counts) {
  R_xlen_t n_lvls = Rf_length(lvls);
  R_xlen_t x_length = Rf_length(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_lvls));

  for (R_xlen_t i = 0; i < n_lvls; ++i) {
    R_xlen_t n_val = INTEGER_ELT(counts, i);

    SEXP dummy = PROTECT(Rf_allocVector(VECSXP, 4));

    SEXP dummy_val = Rf_allocVector(INTSXP, n_val);
    SET_VECTOR_ELT(dummy, 0, dummy_val);
    int* v_dummy_val = INTEGER(dummy_val);

    SEXP dummy_pos = Rf_allocVector(INTSXP, n_val);
    SET_VECTOR_ELT(dummy, 1, dummy_pos);
    int* v_dummy_pos = INTEGER(dummy_pos);

    SEXP dummy_length = Rf_ScalarInteger((int) x_length);
    SET_VECTOR_ELT(dummy, 2, dummy_length);

    SEXP dummy_default = Rf_ScalarInteger((int) 0);
    SET_VECTOR_ELT(dummy, 3, dummy_default);

    R_xlen_t pos_index = 0;
    for (R_xlen_t j = 0; j < x_length; ++j) {
      if (i == INTEGER_ELT(x, j)) {
        v_dummy_val[pos_index] = 1;
        v_dummy_pos[pos_index] = j;
        if (pos_index == n_val) {
          break;
        }
        ++pos_index;
      }
    }
    SET_VECTOR_ELT(out, i, ffi_altrep_new_sparse_integer(dummy));
  }

  return out;
}
