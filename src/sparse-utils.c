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

static inline SEXP altrep_package(SEXP x) {
  return VECTOR_ELT(Rf_PairToVectorList(ATTRIB(ALTREP_CLASS(x))), 1);
}

SEXP ffi_is_sparse_vector(SEXP x) {
  if (!is_altrep(x)) {
    return (Rf_ScalarLogical(FALSE));
  }

  return Rf_ScalarLogical(altrep_package(x) == Rf_install("sparsevctrs"));
}

SEXP ffi_is_altrep_non_sparse_vector(SEXP x) {
  if (!is_altrep(x)) {
    return (Rf_ScalarLogical(FALSE));
  }

  return Rf_ScalarLogical(altrep_package(x) != Rf_install("sparsevctrs"));
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

void verbose_materialize(void) {
  SEXP option = Rf_GetOption1(Rf_install("sparsevctrs.verbose_materialize"));

  if (!Rf_isNull(option)) {
    if (TYPEOF(option) == LGLSXP) {
      Rprintf("sparsevctrs: Sparse vector materialized\n");
    }
    if (TYPEOF(option) == REALSXP) {
      if (*REAL_RO(option) == 3) {
        Rf_error("sparsevctrs: Sparse vector materialized");
      } else if (*REAL_RO(option) == 2) {
        Rf_warning("sparsevctrs: Sparse vector materialized");
      } else {
        Rprintf("sparsevctrs: Sparse vector materialized\n");
      }
    }
    if (TYPEOF(option) == INTSXP) {
      if (*INTEGER_RO(option) == 3) {
        Rf_error("sparsevctrs: Sparse vector materialized");
      } else if (*INTEGER_RO(option) == 2) {
        Rf_warning("sparsevctrs: Sparse vector materialized");
      } else {
        Rprintf("sparsevctrs: Sparse vector materialized\n");
      }
    }
  }
}

void sort_pos_and_val(SEXP pos, SEXP val) {
  R_xlen_t len = Rf_length(pos);

  // nothing to sort -> stop early
  if (len < 2) {
    return;
  }

  SEXP index = PROTECT(Rf_allocVector(INTSXP, len));
  SEXP sorted_pos = PROTECT(Rf_allocVector(INTSXP, len));

  // Initialize pairs array
  for (R_xlen_t i = 0; i < len; i++) {
    SET_INTEGER_ELT(sorted_pos, i, INTEGER_ELT(pos, i));
    SET_INTEGER_ELT(index, i, i);
  }

  // Sort pairs based on values
  for (int i = 0; i < len - 1; i++) {
    for (int j = 0; j < len - i - 1; j++) {
      if (INTEGER_ELT(sorted_pos, j) > INTEGER_ELT(sorted_pos, j + 1)) {
        // Swap pairs
        int temp_pos = INTEGER_ELT(sorted_pos, j);
        int temp_index = INTEGER_ELT(index, j);

        SET_INTEGER_ELT(sorted_pos, j, INTEGER_ELT(sorted_pos, j + 1));
        SET_INTEGER_ELT(sorted_pos, j + 1, temp_pos);

        SET_INTEGER_ELT(index, j, INTEGER_ELT(index, j + 1));
        SET_INTEGER_ELT(index, j + 1, temp_index);
      }
    }
  }

  for (R_xlen_t i = 0; i < len; i++) {
    SET_INTEGER_ELT(pos, i, INTEGER_ELT(sorted_pos, i));
  }

  if (Rf_isInteger(val)) {
    SEXP sorted_val = PROTECT(Rf_allocVector(INTSXP, len));

    for (R_xlen_t i = 0; i < len; i++) {
      int cur_index = INTEGER_ELT(index, i);

      SET_INTEGER_ELT(sorted_val, i, INTEGER_ELT(val, cur_index));
    }

    for (R_xlen_t i = 0; i < len; i++) {
      SET_INTEGER_ELT(val, i, INTEGER_ELT(sorted_val, i));
    }
  } else {
    SEXP sorted_val = PROTECT(Rf_allocVector(REALSXP, len));

    for (R_xlen_t i = 0; i < len; i++) {
      int cur_index = INTEGER_ELT(index, i);

      SET_REAL_ELT(sorted_val, i, REAL_ELT(val, cur_index));
    }

    for (R_xlen_t i = 0; i < len; i++) {
      SET_REAL_ELT(val, i, REAL_ELT(sorted_val, i));
    }
  }
  UNPROTECT(3);
}
