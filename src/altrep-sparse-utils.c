#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP extract_val(SEXP vec) {
  SEXP data1 = R_altrep_data1(vec);
  SEXP out = VECTOR_ELT(data1, 0);
  return out;
}

SEXP extract_pos(SEXP vec) {
  SEXP data1 = R_altrep_data1(vec);
  SEXP out = VECTOR_ELT(data1, 1);
  return out;
}

R_xlen_t extract_len(SEXP vec) {
  SEXP data1 = R_altrep_data1(vec);
  SEXP len = VECTOR_ELT(data1, 2);

  R_xlen_t out = (R_xlen_t) INTEGER_ELT(len, 0);

  return out;
}
