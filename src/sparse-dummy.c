#include "sparse-dummy.h"

// Defined in altrep-sparse-integer.c
extern SEXP ffi_altrep_new_sparse_integer(SEXP);
extern void sparsevctrs_init_altrep_sparse_integer(DllInfo*);

SEXP create_dummy(SEXP pos, R_xlen_t length) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  R_xlen_t pos_len = Rf_length(pos);

  SEXP out_val = Rf_allocVector(INTSXP, pos_len);
  SET_VECTOR_ELT(out, 0, out_val);
  int* v_out_val = INTEGER(out_val);

  for (R_xlen_t i = 0; i < pos_len; ++i) {
    v_out_val[i] = 1;
  }

  SET_VECTOR_ELT(out, 1, pos);

  SEXP out_length = Rf_ScalarInteger((int) length);
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP out_default = Rf_ScalarInteger(0);
  SET_VECTOR_ELT(out, 3, out_default);

  UNPROTECT(1);

  return ffi_altrep_new_sparse_integer(out);
}

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

  for (R_xlen_t i = 0; i < n_lvls; ++i) {
    SEXP pos = VECTOR_ELT(out, i);
    SEXP dummy = create_dummy(pos, x_length);
    SET_VECTOR_ELT(out, i, dummy);
  }

  UNPROTECT(2);

  return out;
}
