#include "altrep-sparse-utils.h"

SEXP extract_val(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  SEXP out = VECTOR_ELT(data1, 0);
  return out;
}

SEXP extract_pos(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  SEXP out = VECTOR_ELT(data1, 1);
  return out;
}

R_xlen_t extract_len(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  SEXP len = VECTOR_ELT(data1, 2);

  R_xlen_t out = (R_xlen_t) INTEGER_ELT(len, 0);

  return out;
}

bool is_altrep(SEXP x) {
  return (bool) ALTREP(x);
}

SEXP ffi_extract_altrep_class(SEXP x) {
  if (!is_altrep(x)) {
    return (R_NilValue);
  }

  return ATTRIB(ALTREP_CLASS(x));
}

static inline R_xlen_t midpoint(R_xlen_t lhs, R_xlen_t rhs) {
  return lhs + (rhs - lhs) / 2;
}

R_xlen_t binary_search(int needle, const int* v_haystack, R_xlen_t size) {
  R_xlen_t loc_lower_bound = 0;
  R_xlen_t loc_upper_bound = size - 1;

  while (loc_lower_bound <= loc_upper_bound) {
    const R_xlen_t loc_middle_bound =
        midpoint(loc_lower_bound, loc_upper_bound);
    const R_xlen_t haystack = v_haystack[loc_middle_bound] - 1;

    if (needle == haystack) {
      return loc_middle_bound;
    } else if (needle < haystack) {
      loc_upper_bound = loc_middle_bound - 1;
    } else {
      // needle > haystack
      loc_lower_bound = loc_middle_bound + 1;
    }
  }

  return size;
}
