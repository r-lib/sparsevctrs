#include "sparse-dummy.h"

// Defined in altrep-sparse-integer.c
extern SEXP ffi_altrep_new_sparse_integer(SEXP);
extern void sparsevctrs_init_altrep_sparse_integer(DllInfo*);

SEXP ffi_sparse_dummy(SEXP x, SEXP lvls, SEXP counts) {
  R_xlen_t n_lvls = Rf_length(lvls);
  R_xlen_t x_length = Rf_length(x);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_lvls));
  SEXP pos_index = PROTECT(Rf_allocVector(INTSXP, n_lvls));
  int* v_pos_index = INTEGER(pos_index);

  for (R_xlen_t i = 0; i < n_lvls; ++i) {
    R_xlen_t n_val = INTEGER_ELT(counts, i);
    SET_VECTOR_ELT(out, i, Rf_allocVector(INTSXP, n_val));
    SET_INTEGER_ELT(pos_index, i, 0);
  }

  for (R_xlen_t i = 0; i < x_length; ++i) {
    int current_val = INTEGER_ELT(x, i);

    int pos = v_pos_index[current_val - 1];

    SEXP pos_vec = VECTOR_ELT(out, current_val - 1);
    int* v_pos_vec = INTEGER(pos_vec);

    v_pos_vec[pos] = i + 1;
    v_pos_index[current_val - 1]++;
  }

  UNPROTECT(2);

  return out;
}
