#include "sparse-arithmatic.h"
#include "sparse-utils.h"

// Defined in altrep-sparse-integer.c
extern SEXP ffi_altrep_new_sparse_integer(SEXP);
extern void sparsevctrs_init_altrep_sparse_integer(DllInfo*);

SEXP empty_sparse_integer(R_xlen_t len) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  SEXP out_val = Rf_allocVector(INTSXP, 0);
  SET_VECTOR_ELT(out, 0, out_val);

  SEXP out_pos = Rf_allocVector(INTSXP, 0);
  SET_VECTOR_ELT(out, 1, out_pos);

  SEXP out_length = Rf_ScalarInteger((int) len);
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP out_default = Rf_ScalarInteger(0);
  SET_VECTOR_ELT(out, 3, out_default);

  SEXP altrep = ffi_altrep_new_sparse_integer(out);

  UNPROTECT(1);
  return altrep;
}

// Defined in altrep-sparse-integer.c
extern SEXP ffi_altrep_new_sparse_double(SEXP);
extern void sparsevctrs_init_altrep_sparse_double(DllInfo*);

SEXP empty_sparse_double(R_xlen_t len) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  SEXP out_val = Rf_allocVector(REALSXP, 0);
  SET_VECTOR_ELT(out, 0, out_val);

  SEXP out_pos = Rf_allocVector(INTSXP, 0);
  SET_VECTOR_ELT(out, 1, out_pos);

  SEXP out_length = Rf_ScalarInteger((int) len);
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP out_default = Rf_ScalarReal(0);
  SET_VECTOR_ELT(out, 3, out_default);

  SEXP altrep = ffi_altrep_new_sparse_double(out);

  UNPROTECT(1);
  return altrep;
}

SEXP find_overlap(SEXP x, SEXP y) {
  SEXP x_pos = extract_pos(x);
  SEXP y_pos = extract_pos(y);
  R_xlen_t x_len = Rf_length(x_pos);
  R_xlen_t y_len = Rf_length(y_pos);

  // special case if x or y have length zero
  if (x_len == 0 || y_len == 0) {
    return R_NilValue;
  }

  SEXP x_matches = Rf_allocVector(INTSXP, x_len);
  SEXP y_matches = Rf_allocVector(INTSXP, y_len);
  for (R_xlen_t i = 0; i < x_len; i++) {
    SET_LOGICAL_ELT(x_matches, i, 0);
  }
  for (R_xlen_t i = 0; i < y_len; i++) {
    SET_LOGICAL_ELT(y_matches, i, 0);
  }

  const int* x_pos_v = INTEGER_RO(x_pos);
  const int* y_pos_v = INTEGER_RO(y_pos);

  R_xlen_t i = 0, j = 0, n = 0;

  while (i < x_len && j < y_len) {
    if (x_pos_v[i] < y_pos_v[j]) {
      i++;
    } else if (x_pos_v[i] > y_pos_v[j]) {
      j++;
    } else {
      n++;
      SET_LOGICAL_ELT(x_matches, i, 1);
      SET_LOGICAL_ELT(y_matches, j, 1);

      i++;
      j++;
    }
  }

  if (n == 0) {
    return R_NilValue;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  SEXP out_x = Rf_allocVector(INTSXP, n);
  SET_VECTOR_ELT(out, 0, out_x);
  int* v_out_x = INTEGER(out_x);

  SEXP out_y = Rf_allocVector(INTSXP, n);
  SET_VECTOR_ELT(out, 1, out_y);
  int* v_out_y = INTEGER(out_y);

  R_xlen_t loc = 0;
  for (R_xlen_t i = 0; i < x_len; i++) {
    if (INTEGER_ELT(x_matches, i) == 1) {
      v_out_x[loc] = i;
      loc++;
    }
  }
  loc = 0;
  for (R_xlen_t i = 0; i < y_len; i++) {
    if (INTEGER_ELT(y_matches, i) == 1) {
      v_out_y[loc] = i;
      loc++;
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP multiplication_doubles_sparse_sparse(SEXP x, SEXP y) {
  SEXP overlap = find_overlap(x, y);
  if (overlap == R_NilValue) {
    return empty_sparse_double(extract_len(x));
  }

  SEXP x_pos_idx = VECTOR_ELT(overlap, 0);
  SEXP y_pos_idx = VECTOR_ELT(overlap, 1);

  SEXP x_pos = extract_pos(x);
  SEXP x_val = extract_val(x);

  SEXP y_val = extract_val(y);

  R_xlen_t out_len = Rf_length(x_pos_idx);
  SEXP out_pos = Rf_allocVector(INTSXP, out_len);
  SEXP out_val = Rf_allocVector(REALSXP, out_len);

  for (R_xlen_t i = 0; i < out_len; i++) {
    SET_INTEGER_ELT(out_pos, i, INTEGER_ELT(x_pos, INTEGER_ELT(x_pos_idx, i)));
    SET_REAL_ELT(
        out_val,
        i,
        REAL_ELT(x_val, INTEGER_ELT(x_pos_idx, i)) *
            REAL_ELT(y_val, INTEGER_ELT(y_pos_idx, i))
    );
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(out, 0, out_val);

  SET_VECTOR_ELT(out, 1, out_pos);

  SEXP out_length = Rf_ScalarInteger((int) extract_len(x));
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP out_default = Rf_ScalarReal(0);
  SET_VECTOR_ELT(out, 3, out_default);

  SEXP altrep = ffi_altrep_new_sparse_double(out);

  UNPROTECT(1);
  return altrep;
}

SEXP multiplication_doubles_sparse_dense(SEXP x, SEXP y) {
  SEXP x_pos = extract_pos(x);
  SEXP x_val = extract_val(x);
  R_xlen_t x_len = extract_len(x);
  R_xlen_t n_values = Rf_length(x_pos);

  R_xlen_t n_zero = 0;
  for (R_xlen_t i = 0; i < n_values; i++) {
    if (REAL_ELT(y, INTEGER_ELT(x_pos, i) - 1) == 0) {
      n_zero++;
    }
  }

  R_xlen_t out_len = n_values - n_zero;
  SEXP out_pos = Rf_allocVector(INTSXP, out_len);
  SEXP out_val = Rf_allocVector(REALSXP, out_len);

  R_xlen_t idx = 0;

  for (R_xlen_t i = 0; i < n_values; i++) {
    int cur_pos = INTEGER_ELT(x_pos, i);
    double y_val = REAL_ELT(y, cur_pos - 1);

    if (y_val != 0) {
      SET_INTEGER_ELT(out_pos, idx, cur_pos);
      double cur_val = REAL_ELT(x_val, i);
      SET_REAL_ELT(out_val, idx, y_val * cur_val);
      idx++;
    }
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(out, 0, out_val);

  SET_VECTOR_ELT(out, 1, out_pos);

  SEXP out_length = Rf_ScalarInteger((int) x_len);
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP out_default = Rf_ScalarReal(0);
  SET_VECTOR_ELT(out, 3, out_default);

  SEXP altrep = ffi_altrep_new_sparse_double(out);

  UNPROTECT(1);
  return altrep;
}

SEXP multiplication_doubles_dense_dense(SEXP x, SEXP y) {
  R_xlen_t len = Rf_length(x);

  SEXP out = Rf_allocVector(REALSXP, len);

  for (R_xlen_t i = 0; i < len; i++) {
    SET_REAL_ELT(out, i, REAL_ELT(x, i) * REAL_ELT(y, i));
  }

  return out;
}

SEXP multiplication_doubles(SEXP x, SEXP y) {
  if (is_altrep(x)) {
    if (is_altrep(y)) {
      return multiplication_doubles_sparse_sparse(x, y);
    } else {
      return multiplication_doubles_sparse_dense(x, y);
    }
  } else {
    if (is_altrep(y)) {
      return multiplication_doubles_sparse_dense(y, x);
    } else {
      return multiplication_doubles_dense_dense(x, y);
    }
  }
  return x;
}

SEXP multiplication_integers_sparse_sparse(SEXP x, SEXP y) {
  SEXP overlap = find_overlap(x, y);
  if (overlap == R_NilValue) {
    return empty_sparse_integer(extract_len(x));
  }

  SEXP x_pos_idx = VECTOR_ELT(overlap, 0);
  SEXP y_pos_idx = VECTOR_ELT(overlap, 1);

  SEXP x_pos = extract_pos(x);
  SEXP x_val = extract_val(x);

  SEXP y_val = extract_val(y);

  R_xlen_t out_len = Rf_length(x_pos_idx);
  SEXP out_pos = Rf_allocVector(INTSXP, out_len);
  SEXP out_val = Rf_allocVector(INTSXP, out_len);

  for (R_xlen_t i = 0; i < out_len; i++) {
    SET_INTEGER_ELT(out_pos, i, INTEGER_ELT(x_pos, INTEGER_ELT(x_pos_idx, i)));
    SET_INTEGER_ELT(
        out_val,
        i,
        INTEGER_ELT(x_val, INTEGER_ELT(x_pos_idx, i)) *
            INTEGER_ELT(y_val, INTEGER_ELT(y_pos_idx, i))
    );
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(out, 0, out_val);

  SET_VECTOR_ELT(out, 1, out_pos);

  SEXP out_length = Rf_ScalarInteger((int) extract_len(x));
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP out_default = Rf_ScalarInteger(0);
  SET_VECTOR_ELT(out, 3, out_default);

  SEXP altrep = ffi_altrep_new_sparse_integer(out);

  UNPROTECT(1);
  return altrep;
}

SEXP multiplication_integers_sparse_dense(SEXP x, SEXP y) {
  SEXP x_pos = extract_pos(x);
  SEXP x_val = extract_val(x);
  R_xlen_t x_len = extract_len(x);
  R_xlen_t n_values = Rf_length(x_pos);

  R_xlen_t n_zero = 0;
  for (R_xlen_t i = 0; i < n_values; i++) {
    if (INTEGER_ELT(y, INTEGER_ELT(x_pos, i) - 1) == 0) {
      n_zero++;
    }
  }

  R_xlen_t out_len = n_values - n_zero;
  SEXP out_pos = Rf_allocVector(INTSXP, out_len);
  SEXP out_val = Rf_allocVector(INTSXP, out_len);

  R_xlen_t idx = 0;

  for (R_xlen_t i = 0; i < n_values; i++) {
    int cur_pos = INTEGER_ELT(x_pos, i);
    int y_val = INTEGER_ELT(y, cur_pos - 1);

    if (y_val != 0) {
      SET_INTEGER_ELT(out_pos, idx, cur_pos);
      int cur_val = INTEGER_ELT(x_val, i);
      SET_INTEGER_ELT(out_val, idx, y_val * cur_val);
      idx++;
    }
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));

  SET_VECTOR_ELT(out, 0, out_val);

  SET_VECTOR_ELT(out, 1, out_pos);

  SEXP out_length = Rf_ScalarInteger((int) x_len);
  SET_VECTOR_ELT(out, 2, out_length);

  SEXP out_default = Rf_ScalarInteger(0);
  SET_VECTOR_ELT(out, 3, out_default);

  SEXP altrep = ffi_altrep_new_sparse_integer(out);

  UNPROTECT(1);
  return altrep;
}

SEXP multiplication_integers_dense_dense(SEXP x, SEXP y) {
  R_xlen_t len = Rf_length(x);

  SEXP out = Rf_allocVector(INTSXP, len);

  for (R_xlen_t i = 0; i < len; i++) {
    SET_INTEGER_ELT(out, i, INTEGER_ELT(x, i) * INTEGER_ELT(y, i));
  }

  return out;
}

SEXP multiplication_integers(SEXP x, SEXP y) {
  if (is_altrep(x)) {
    if (is_altrep(y)) {
      return multiplication_integers_sparse_sparse(x, y);
    } else {
      return multiplication_integers_sparse_dense(x, y);
    }
  } else {
    if (is_altrep(y)) {
      return multiplication_integers_sparse_dense(y, x);
    } else {
      return multiplication_integers_dense_dense(x, y);
    }
  }
  return x;
}

SEXP ffi_sparse_multiplication(SEXP x, SEXP y) {
  SEXP out;

  if (Rf_isInteger(x)) {
    out = multiplication_integers(x, y);
  } else {
    out = multiplication_doubles(x, y);
  }

  return out;
}
