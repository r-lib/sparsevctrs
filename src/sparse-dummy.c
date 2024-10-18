#include "sparse-dummy.h"

// Defined in altrep-sparse-integer.c
extern SEXP ffi_altrep_new_sparse_integer(SEXP);
extern void sparsevctrs_init_altrep_sparse_integer(DllInfo*);

SEXP create_dummy(SEXP pos, R_xlen_t length) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  const R_xlen_t pos_len = Rf_length(pos);

  // values
  SEXP out_val = Rf_allocVector(INTSXP, pos_len);
  SET_VECTOR_ELT(out, 0, out_val);
  int* v_out_val = INTEGER(out_val);

  for (R_xlen_t i = 0; i < pos_len; ++i) {
    v_out_val[i] = 1;
  }

  // positions
  SET_VECTOR_ELT(out, 1, pos);

  // length
  const SEXP out_length = Rf_ScalarInteger((int) length);
  SET_VECTOR_ELT(out, 2, out_length);

  // default
  const SEXP out_default = Rf_ScalarInteger(0);
  SET_VECTOR_ELT(out, 3, out_default);

  UNPROTECT(1);

  return ffi_altrep_new_sparse_integer(out);
}

SEXP ffi_sparse_dummy(SEXP x, SEXP lvls, SEXP counts, SEXP one_hot) {
  const R_xlen_t n_lvls = Rf_length(lvls);
  const R_xlen_t len = Rf_length(x);

  const int* v_x = INTEGER_RO(x);

  // Generate list of integer vectors. One vector for each level, with its
  // length equal to the number of occurances of that level.
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_lvls));

  for (R_xlen_t i = 0; i < n_lvls; ++i) {
    R_xlen_t n_val = INTEGER_ELT(counts, i);
    SET_VECTOR_ELT(out, i, Rf_allocVector(INTSXP, n_val));
  }

  // Vector of positions to keep track of how far into each position vector we
  // are. Initialize to 0 to indicate first position.
  SEXP pos_index = PROTECT(Rf_allocVector(INTSXP, n_lvls));
  int* v_pos_index = INTEGER(pos_index);
  for (R_xlen_t i = 0; i < n_lvls; ++i) {
    SET_INTEGER_ELT(pos_index, i, 0);
  }

  // Itterate over input, find its position index, and place the position value
  // at the index. Increment specific index.

  if ((bool) one_hot) {
    for (R_xlen_t i = 0; i < len; ++i) {
      int current_val = v_x[i] - 1;

      if (current_val == -1) {
        continue;
      }

      int index = v_pos_index[current_val];

      SEXP pos_vec = VECTOR_ELT(out, current_val);
      int* v_pos_vec = INTEGER(pos_vec);

      // we need the result to be 1-indexed
      v_pos_vec[index] = i + 1;
      v_pos_index[current_val]++;
    }
  } else {
    for (R_xlen_t i = 0; i < len; ++i) {
      int current_val = v_x[i] - 1;

      int index = v_pos_index[current_val];

      SEXP pos_vec = VECTOR_ELT(out, current_val);
      int* v_pos_vec = INTEGER(pos_vec);

      // we need the result to be 1-indexed
      v_pos_vec[index] = i + 1;
      v_pos_index[current_val]++;
    }
  }

  // Turn list of integer vectors with positions, into list of sparse integer
  // vectors.
  for (R_xlen_t i = 0; i < n_lvls; ++i) {
    SEXP positions = VECTOR_ELT(out, i);
    SEXP dummy = create_dummy(positions, len);
    SET_VECTOR_ELT(out, i, dummy);
  }

  UNPROTECT(2);

  return out;
}
