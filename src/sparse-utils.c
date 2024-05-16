#include "sparse-utils.h"

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

SEXP extract_default(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  SEXP out = VECTOR_ELT(data1, 3);

  return out;
}

double extract_default_double(SEXP x) {
  SEXP default_val = extract_default(x);
  double out = REAL_ELT(default_val, 0);

  return out;
}

int extract_default_integer(SEXP x) {
  SEXP default_val = extract_default(x);
  int out = INTEGER_ELT(default_val, 0);

  return out;
}

SEXP extract_default_string(SEXP x) {
  SEXP default_val = extract_default(x);
  SEXP out = STRING_ELT(default_val, 0);

  return out;
}

Rboolean extract_default_logical(SEXP x) {
  SEXP default_val = extract_default(x);
  Rboolean out = LOGICAL_ELT(default_val, 0);

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
    const R_xlen_t haystack = v_haystack[loc_middle_bound];

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

bool is_index_handleable(SEXP x) {
  if (TYPEOF(x) != INTSXP) {
    // i.e. can't handle indexing for long vectors
    return false;
  }

  R_xlen_t size = Rf_xlength(x);
  const int* v_x = INTEGER_RO(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    const int elt = v_x[i];

    if (elt == NA_INTEGER) {
      continue;
    }
    if (elt == 0) {
      // `0` indices would create a result with a size `< length(indx)`, and we
      // can't easily handle that right now
      return false;
    }
    if (elt < 0) {
      // Pathological case, should never happen
      return false;
    }
  }

  return true;
}
