#include "sparsevctrs-core.h"

#include "altrep-sparse-double.h"

SEXP ffi_sparse_matrix_to_list(SEXP i, SEXP p, SEXP x, SEXP nrow) {
  const int* v_i = INTEGER_RO(i);
  const int* v_p = INTEGER_RO(p);
  const double* v_x = x == R_NilValue ? NULL : REAL_RO(x);

  const R_xlen_t n_col = Rf_xlength(p) - 1;

  SEXP out = PROTECT(Rf_allocVector(VECSXP, n_col));

  // Shared between all columns
  SEXP len = PROTECT(Rf_ScalarInteger(INTEGER_ELT(nrow, 0)));
  SEXP def = PROTECT(Rf_ScalarReal(0));
  MARK_NOT_MUTABLE(len);
  MARK_NOT_MUTABLE(def);

  for (R_xlen_t col = 0; col < n_col; ++col) {
    const int start = v_p[col];
    const R_xlen_t n = v_p[col + 1] - start;

    SEXP val = PROTECT(Rf_allocVector(REALSXP, n));
    double* v_val = REAL(val);

    SEXP pos = PROTECT(Rf_allocVector(INTSXP, n));
    int* v_pos = INTEGER(pos);

    for (R_xlen_t j = 0; j < n; ++j) {
      v_val[j] = v_x == NULL ? 1 : v_x[start + j];
      v_pos[j] = v_i[start + j] + 1;
    }

    SET_VECTOR_ELT(out, col, new_sparse_double(val, pos, len, def));
    UNPROTECT(2);
  }

  UNPROTECT(3);
  return out;
}
