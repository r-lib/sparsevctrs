#include "sparse-arithmatic.h"
#include "sparse-utils.h"
#include "altrep-sparse-double.h"
#include "altrep-sparse-integer.h"

SEXP empty_sparse_integer(R_xlen_t len) {
  SEXP out = new_sparse_integer(
      PROTECT(Rf_allocVector(INTSXP, 0)),
      PROTECT(Rf_allocVector(INTSXP, 0)),
      PROTECT(Rf_ScalarInteger((int) len)),
      PROTECT(Rf_ScalarInteger(0))
  );

  UNPROTECT(4);
  return out;
}

SEXP empty_sparse_double(R_xlen_t len) {
  SEXP out = new_sparse_double(
      PROTECT(Rf_allocVector(REALSXP, 0)),
      PROTECT(Rf_allocVector(INTSXP, 0)),
      PROTECT(Rf_ScalarInteger((int) len)),
      PROTECT(Rf_ScalarReal(0))
  );

  UNPROTECT(4);
  return out;
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

  SEXP x_matches = PROTECT(Rf_allocVector(INTSXP, x_len));
  SEXP y_matches = PROTECT(Rf_allocVector(INTSXP, y_len));
  for (R_xlen_t i = 0; i < x_len; i++) {
    SET_INTEGER_ELT(x_matches, i, 0);
  }
  for (R_xlen_t i = 0; i < y_len; i++) {
    SET_INTEGER_ELT(y_matches, i, 0);
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
      SET_INTEGER_ELT(x_matches, i, 1);
      SET_INTEGER_ELT(y_matches, j, 1);

      i++;
      j++;
    }
  }

  if (n == 0) {
    UNPROTECT(2);
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

  UNPROTECT(3);
  return out;
}

Rboolean int_match(int x, SEXP table) {
  R_xlen_t table_len = Rf_length(table);

  Rboolean out = FALSE;

  for (R_xlen_t i = 0; i < table_len; i++) {
    if (INTEGER_ELT(table, i) == x) {
      out = TRUE;
      break;
    }
  }
  return out;
}

SEXP find_nas_with_no_overlap(SEXP x, SEXP y) {
  SEXP x_pos = extract_pos(x);
  SEXP x_val = extract_val(x);
  SEXP y_pos = extract_pos(y);
  SEXP y_val = extract_val(y);
  R_xlen_t x_len = Rf_length(x_pos);
  R_xlen_t y_len = Rf_length(y_pos);

  // special case if x or y have length zero
  if (x_len == 0 || y_len == 0) {
    return R_NilValue;
  }

  SEXP x_matches = PROTECT(Rf_allocVector(INTSXP, x_len));
  SEXP y_matches = PROTECT(Rf_allocVector(INTSXP, y_len));

  for (R_xlen_t i = 0; i < x_len; i++) {
    if (Rf_isInteger(x_val)) {
      if (INTEGER_ELT(x_val, i) == NA_INTEGER) {
        SET_INTEGER_ELT(x_matches, i, 1);
      } else {
        SET_INTEGER_ELT(x_matches, i, 0);
      }
    } else {
      if (R_IsNA(REAL_ELT(x_val, i))) {
        SET_INTEGER_ELT(x_matches, i, 1);
      } else {
        SET_INTEGER_ELT(x_matches, i, 0);
      }
    }
  }

  for (R_xlen_t i = 0; i < y_len; i++) {
    if (Rf_isInteger(y_val)) {
      if (INTEGER_ELT(y_val, i) == NA_INTEGER) {
        SET_INTEGER_ELT(y_matches, i, 1);
      } else {
        SET_INTEGER_ELT(y_matches, i, 0);
      }
    } else {
      if (R_IsNA(REAL_ELT(y_val, i))) {
        SET_INTEGER_ELT(y_matches, i, 1);
      } else {
        SET_INTEGER_ELT(y_matches, i, 0);
      }
    }
  }

  const int* x_pos_v = INTEGER_RO(x_pos);
  const int* y_pos_v = INTEGER_RO(y_pos);

  R_xlen_t i = 0, j = 0;

  while (i < x_len && j < y_len) {
    if (x_pos_v[i] < y_pos_v[j]) {
      i++;
    } else if (x_pos_v[i] > y_pos_v[j]) {
      j++;
    } else {
      SET_INTEGER_ELT(x_matches, i, 0);
      SET_INTEGER_ELT(y_matches, j, 0);

      i++;
      j++;
    }
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  R_xlen_t n_x = 0;
  for (R_xlen_t i = 0; i < x_len; i++) {
    if (INTEGER_ELT(x_matches, i) == 1) {
      n_x++;
    }
  }
  R_xlen_t n_y = 0;
  for (R_xlen_t i = 0; i < y_len; i++) {
    if (INTEGER_ELT(y_matches, i) == 1) {
      n_y++;
    }
  }

  SEXP out_x = Rf_allocVector(INTSXP, n_x);
  SET_VECTOR_ELT(out, 0, out_x);
  int* v_out_x = INTEGER(out_x);

  SEXP out_y = Rf_allocVector(INTSXP, n_y);
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

  UNPROTECT(3);
  return out;
}

SEXP multiplication_doubles_sparse_sparse(SEXP x, SEXP y) {
  SEXP overlap = PROTECT(find_overlap(x, y));
  SEXP nas = find_nas_with_no_overlap(x, y);

  SEXP x_val = extract_val(x);
  SEXP x_pos = extract_pos(x);
  SEXP y_val = extract_val(y);
  SEXP y_pos = extract_pos(y);

  SEXP x_na_pos = VECTOR_ELT(nas, 0);
  SEXP y_na_pos = VECTOR_ELT(nas, 1);

  int x_na_count = Rf_length(x_na_pos);
  int y_na_count = Rf_length(y_na_pos);

  if (overlap == R_NilValue && x_na_count == 0 && y_na_count == 0) {
    return empty_sparse_double(extract_len(x));
  }

  R_xlen_t n_overlap = 0;
  if (overlap != R_NilValue) {
    n_overlap = Rf_length(VECTOR_ELT(overlap, 0));
  }

  R_xlen_t out_len = n_overlap + x_na_count + y_na_count;

  SEXP out_pos = PROTECT(Rf_allocVector(INTSXP, out_len));
  SEXP out_val = PROTECT(Rf_allocVector(REALSXP, out_len));
  R_xlen_t out_idx = 0;

  if (overlap != R_NilValue) {
    SEXP x_pos_idx = VECTOR_ELT(overlap, 0);
    SEXP y_pos_idx = VECTOR_ELT(overlap, 1);

    SEXP x_pos = extract_pos(x);

    for (R_xlen_t i = 0; i < n_overlap; i++) {
      SET_INTEGER_ELT(
          out_pos, i, INTEGER_ELT(x_pos, INTEGER_ELT(x_pos_idx, i))
      );

      SET_REAL_ELT(
          out_val,
          out_idx,
          REAL_ELT(x_val, INTEGER_ELT(x_pos_idx, i)) *
              REAL_ELT(y_val, INTEGER_ELT(y_pos_idx, i))
      );
      out_idx++;
    }
  }

  // set x NA values
  for (R_xlen_t i = 0; i < x_na_count; i++) {
    SET_INTEGER_ELT(
        out_pos, out_idx, INTEGER_ELT(x_pos, INTEGER_ELT(x_na_pos, i))
    );
    SET_REAL_ELT(out_val, out_idx, NA_REAL);
    out_idx++;
  }

  // set y NA values
  for (R_xlen_t i = 0; i < y_na_count; i++) {
    SET_INTEGER_ELT(
        out_pos, out_idx, INTEGER_ELT(y_pos, INTEGER_ELT(y_na_pos, i))
    );
    SET_REAL_ELT(out_val, out_idx, NA_REAL);
    out_idx++;
  }

  sort_pos_and_val(out_pos, out_val);

  SEXP out_length = PROTECT(Rf_ScalarInteger((int) extract_len(x)));
  SEXP out_default = PROTECT(Rf_ScalarReal(0));

  SEXP out = new_sparse_double(out_val, out_pos, out_length, out_default);

  UNPROTECT(5);

  return out;
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

  // Locate NA values for Y - dense
  R_xlen_t n_y_nas = 0;
  R_xlen_t y_len = Rf_length(y);
  for (R_xlen_t i = 0; i < y_len; i++) {
    if (R_IsNA(REAL_ELT(y, i))) {
      // i + 1 because of R-indexing
      if (!int_match((int) i + 1, x_pos)) {
        n_y_nas++;
      }
    }
  }

  SEXP y_na_pos = PROTECT(Rf_allocVector(INTSXP, n_y_nas));
  R_xlen_t idx = 0;

  for (R_xlen_t i = 0; i < y_len; i++) {
    if (R_IsNA(REAL_ELT(y, i))) {
      // i + 1 because of R-indexing
      if (!int_match((int) i + 1, x_pos)) {
        SET_INTEGER_ELT(y_na_pos, idx, i);
        idx++;
      }
    }
  }

  // Locate NA values for X sparse
  R_xlen_t n_x_nas = 0;
  for (R_xlen_t i = 0; i < n_values; i++) {
    if (R_IsNA(REAL_ELT(x_val, i))) {
      int cur_pos = INTEGER_ELT(x_pos, i);
      // cur_pos - 1 because of R-indexing
      int cur_y_val = REAL_ELT(y, cur_pos - 1);

      if (cur_y_val == 0) {
        n_x_nas++;
      }
    }
  }

  SEXP x_na_pos = PROTECT(Rf_allocVector(INTSXP, n_x_nas));
  idx = 0;

  for (R_xlen_t i = 0; i < n_values; i++) {
    if (R_IsNA(REAL_ELT(x_val, i))) {
      int cur_pos = INTEGER_ELT(x_pos, i);
      // cur_pos - 1 because of R-indexing
      int cur_y_val = REAL_ELT(y, cur_pos - 1);

      if (cur_y_val == 0) {
        SET_INTEGER_ELT(x_na_pos, idx, cur_pos);
        idx++;
      }
    }
  }

  R_xlen_t out_len = n_values - n_zero + n_x_nas + n_y_nas;
  SEXP out_pos = PROTECT(Rf_allocVector(INTSXP, out_len));
  SEXP out_val = PROTECT(Rf_allocVector(REALSXP, out_len));

  idx = 0;

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

  // set x na values
  for (R_xlen_t i = 0; i < n_x_nas; i++) {
    int cur_pos = INTEGER_ELT(x_na_pos, i);

    SET_INTEGER_ELT(out_pos, idx, cur_pos);
    SET_REAL_ELT(out_val, idx, NA_REAL);
    idx++;
  }

  // set y na values
  for (R_xlen_t i = 0; i < n_y_nas; i++) {
    int cur_pos = INTEGER_ELT(y_na_pos, i);

    // cur_pos + 1 because it is R-indexed
    SET_INTEGER_ELT(out_pos, idx, cur_pos + 1);
    SET_REAL_ELT(out_val, idx, NA_REAL);
    idx++;
  }

  sort_pos_and_val(out_pos, out_val);

  SEXP out_length = PROTECT(Rf_ScalarInteger((int) x_len));
  SEXP out_default = PROTECT(Rf_ScalarReal(0));

  SEXP out = new_sparse_double(out_val, out_pos, out_length, out_default);

  UNPROTECT(6);
  return out;
}

SEXP multiplication_doubles_dense_dense(SEXP x, SEXP y) {
  R_xlen_t len = Rf_length(x);

  SEXP out = Rf_allocVector(REALSXP, len);

  for (R_xlen_t i = 0; i < len; i++) {
    double x_val = REAL_ELT(x, i);
    double y_val = REAL_ELT(y, i);
    SET_REAL_ELT(out, i, x_val * y_val);
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
  SEXP overlap = PROTECT(find_overlap(x, y));
  SEXP nas = find_nas_with_no_overlap(x, y);

  SEXP x_val = extract_val(x);
  SEXP x_pos = extract_pos(x);
  SEXP y_val = extract_val(y);
  SEXP y_pos = extract_pos(y);

  SEXP x_na_pos = VECTOR_ELT(nas, 0);
  SEXP y_na_pos = VECTOR_ELT(nas, 1);

  int x_na_count = Rf_length(x_na_pos);
  int y_na_count = Rf_length(y_na_pos);

  if (overlap == R_NilValue && x_na_count == 0 && y_na_count == 0) {
    return empty_sparse_integer(extract_len(x));
  }

  R_xlen_t n_overlap = 0;
  if (overlap != R_NilValue) {
    n_overlap = Rf_length(VECTOR_ELT(overlap, 0));
  }

  R_xlen_t out_len = n_overlap + x_na_count + y_na_count;

  SEXP out_pos = PROTECT(Rf_allocVector(INTSXP, out_len));
  SEXP out_val = PROTECT(Rf_allocVector(INTSXP, out_len));
  R_xlen_t out_idx = 0;

  if (overlap != R_NilValue) {
    SEXP x_pos_idx = VECTOR_ELT(overlap, 0);
    SEXP y_pos_idx = VECTOR_ELT(overlap, 1);

    SEXP x_pos = extract_pos(x);

    for (R_xlen_t i = 0; i < n_overlap; i++) {
      SET_INTEGER_ELT(
          out_pos, i, INTEGER_ELT(x_pos, INTEGER_ELT(x_pos_idx, i))
      );
      int cur_x_val = INTEGER_ELT(x_val, INTEGER_ELT(x_pos_idx, i));
      int cur_y_val = INTEGER_ELT(y_val, INTEGER_ELT(y_pos_idx, i));

      if (cur_x_val == NA_INTEGER || cur_y_val == NA_INTEGER) {
        SET_INTEGER_ELT(out_val, out_idx, NA_INTEGER);
      } else {
        SET_INTEGER_ELT(out_val, out_idx, cur_x_val * cur_y_val);
      }

      out_idx++;
    }
  }

  // set x NA values
  for (R_xlen_t i = 0; i < x_na_count; i++) {
    SET_INTEGER_ELT(
        out_pos, out_idx, INTEGER_ELT(x_pos, INTEGER_ELT(x_na_pos, i))
    );
    SET_INTEGER_ELT(out_val, out_idx, NA_INTEGER);
    out_idx++;
  }

  // set y NA values
  for (R_xlen_t i = 0; i < y_na_count; i++) {
    SET_INTEGER_ELT(
        out_pos, out_idx, INTEGER_ELT(y_pos, INTEGER_ELT(y_na_pos, i))
    );
    SET_INTEGER_ELT(out_val, out_idx, NA_INTEGER);
    out_idx++;
  }

  sort_pos_and_val(out_pos, out_val);

  SEXP out_length = PROTECT(Rf_ScalarInteger((int) extract_len(x)));
  SEXP out_default = PROTECT(Rf_ScalarInteger(0));

  SEXP out = new_sparse_integer(out_val, out_pos, out_length, out_default);

  UNPROTECT(5);

  return out;
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

  // Locate NA values for Y - dense
  R_xlen_t n_y_nas = 0;
  R_xlen_t y_len = Rf_length(y);
  for (R_xlen_t i = 0; i < y_len; i++) {
    if (INTEGER_ELT(y, i) == NA_INTEGER) {
      // i + 1 because of R-indexing
      if (!int_match((int) i + 1, x_pos)) {
        n_y_nas++;
      }
    }
  }

  SEXP y_na_pos = PROTECT(Rf_allocVector(INTSXP, n_y_nas));
  R_xlen_t idx = 0;

  for (R_xlen_t i = 0; i < y_len; i++) {
    if (INTEGER_ELT(y, i) == NA_INTEGER) {
      // i + 1 because of R-indexing
      if (!int_match((int) i + 1, x_pos)) {
        SET_INTEGER_ELT(y_na_pos, idx, i);
        idx++;
      }
    }
  }

  // Locate NA values for X sparse
  R_xlen_t n_x_nas = 0;
  for (R_xlen_t i = 0; i < n_values; i++) {
    if (INTEGER_ELT(x_val, i) == NA_INTEGER) {
      int cur_pos = INTEGER_ELT(x_pos, i);
      // cur_pos - 1 because of R-indexing
      int cur_y_val = INTEGER_ELT(y, cur_pos - 1);

      if (cur_y_val == 0) {
        n_x_nas++;
      }
    }
  }

  SEXP x_na_pos = PROTECT(Rf_allocVector(INTSXP, n_x_nas));
  idx = 0;

  for (R_xlen_t i = 0; i < n_values; i++) {
    if (INTEGER_ELT(x_val, i) == NA_INTEGER) {
      int cur_pos = INTEGER_ELT(x_pos, i);
      // cur_pos - 1 because of R-indexing
      int cur_y_val = INTEGER_ELT(y, cur_pos - 1);

      if (cur_y_val == 0) {
        SET_INTEGER_ELT(x_na_pos, idx, cur_pos);
        idx++;
      }
    }
  }

  R_xlen_t out_len = n_values - n_zero + n_x_nas + n_y_nas;
  SEXP out_pos = PROTECT(Rf_allocVector(INTSXP, out_len));
  SEXP out_val = PROTECT(Rf_allocVector(INTSXP, out_len));

  idx = 0;

  for (R_xlen_t i = 0; i < n_values; i++) {
    int cur_pos = INTEGER_ELT(x_pos, i);
    int y_val = INTEGER_ELT(y, cur_pos - 1);

    if (y_val != 0) {
      SET_INTEGER_ELT(out_pos, idx, cur_pos);
      int cur_val = INTEGER_ELT(x_val, i);
      if (y_val == NA_INTEGER || cur_val == NA_INTEGER) {
        SET_INTEGER_ELT(out_val, idx, NA_INTEGER);
      } else {
        SET_INTEGER_ELT(out_val, idx, y_val * cur_val);
      }
      idx++;
    }
  }

  // set x na values
  for (R_xlen_t i = 0; i < n_x_nas; i++) {
    int cur_pos = INTEGER_ELT(x_na_pos, i);

    SET_INTEGER_ELT(out_pos, idx, cur_pos);
    SET_INTEGER_ELT(out_val, idx, NA_INTEGER);
    idx++;
  }

  // set y na values
  for (R_xlen_t i = 0; i < n_y_nas; i++) {
    int cur_pos = INTEGER_ELT(y_na_pos, i);

    // cur_pos + 1 because it is R-indexed
    SET_INTEGER_ELT(out_pos, idx, cur_pos + 1);
    SET_INTEGER_ELT(out_val, idx, NA_INTEGER);
    idx++;
  }

  sort_pos_and_val(out_pos, out_val);

  SEXP out_length = PROTECT(Rf_ScalarInteger((int) x_len));
  SEXP out_default = PROTECT(Rf_ScalarInteger(0));

  SEXP out = new_sparse_integer(out_val, out_pos, out_length, out_default);

  UNPROTECT(6);
  return out;
}

SEXP multiplication_integers_dense_dense(SEXP x, SEXP y) {
  R_xlen_t len = Rf_length(x);

  SEXP out = Rf_allocVector(INTSXP, len);

  for (R_xlen_t i = 0; i < len; i++) {
    int x_val = INTEGER_ELT(x, i);
    int y_val = INTEGER_ELT(y, i);
    if (x_val == NA_INTEGER || y_val == NA_INTEGER) {
      SET_INTEGER_ELT(out, i, NA_INTEGER);
    } else {
      SET_INTEGER_ELT(out, i, x_val * y_val);
    }
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
