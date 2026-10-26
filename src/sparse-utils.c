#include "sparsevctrs-core.h"

#include "sparse-utils.h"

#include "decl/sparse-utils-decl.h"

r_obj* ffi_sparse_extract_altrep_class(r_obj* ffi_x) {
  if (!sparse_is_altrep(ffi_x)) {
    return (r_null);
  }

  return R_altrep_class_name(ffi_x);
}

r_obj* ffi_sparse_is_sparse_vector(r_obj* ffi_x) {
  if (!sparse_is_altrep(ffi_x)) {
    return (r_lgl(false));
  }

  return r_lgl(altrep_package(ffi_x) == r_sym("sparsevctrs"));
}

r_obj* ffi_sparse_is_altrep_non_sparse_vector(r_obj* ffi_x) {
  if (!sparse_is_altrep(ffi_x)) {
    return (r_lgl(false));
  }

  return r_lgl(altrep_package(ffi_x) != r_sym("sparsevctrs"));
}

bool sparse_is_altrep(r_obj* x) {
  return (bool) ALTREP(x);
}

static inline r_obj* altrep_package(r_obj* x) {
  return R_altrep_class_package(x);
}

r_obj* sparse_extract_val(r_obj* x) {
  r_obj* data1 = R_altrep_data1(x);
  r_obj* out = r_list_get(data1, 0);
  return out;
}

r_obj* sparse_extract_pos(r_obj* x) {
  r_obj* data1 = R_altrep_data1(x);
  r_obj* out = r_list_get(data1, 1);
  return out;
}

r_ssize sparse_extract_len(r_obj* x) {
  r_obj* data1 = R_altrep_data1(x);
  r_obj* len = r_list_get(data1, 2);

  const r_ssize out = (r_ssize) r_int_get(len, 0);

  return out;
}

r_obj* sparse_extract_default(r_obj* x) {
  r_obj* data1 = R_altrep_data1(x);
  r_obj* out = r_list_get(data1, 3);

  return out;
}

double sparse_extract_default_double(r_obj* x) {
  r_obj* default_val = sparse_extract_default(x);
  const double out = r_dbl_get(default_val, 0);

  return out;
}

int sparse_extract_default_integer(r_obj* x) {
  r_obj* default_val = sparse_extract_default(x);
  const int out = r_int_get(default_val, 0);

  return out;
}

r_obj* sparse_extract_default_string(r_obj* x) {
  r_obj* default_val = sparse_extract_default(x);
  r_obj* out = r_chr_get(default_val, 0);

  return out;
}

int sparse_extract_default_logical(r_obj* x) {
  r_obj* default_val = sparse_extract_default(x);
  const int out = r_lgl_get(default_val, 0);

  return out;
}

r_ssize sparse_binary_search(int needle, const int* v_haystack, r_ssize size) {
  r_ssize loc_lower_bound = 0;
  r_ssize loc_upper_bound = size - 1;

  while (loc_lower_bound <= loc_upper_bound) {
    const r_ssize loc_middle_bound = midpoint(loc_lower_bound, loc_upper_bound);
    const r_ssize haystack = v_haystack[loc_middle_bound];

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

static inline r_ssize midpoint(r_ssize lhs, r_ssize rhs) {
  return lhs + (rhs - lhs) / 2;
}

bool sparse_is_index_handleable(r_obj* x) {
  if (r_typeof(x) != R_TYPE_integer) {
    // i.e. can't handle indexing for long vectors
    return false;
  }

  const r_ssize size = r_length(x);
  const int* v_x = r_int_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    const int elt = v_x[i];

    if (elt == r_globals.na_int) {
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

void sparse_verbose_materialize(void) {
  r_obj* option = Rf_GetOption1(r_sym("sparsevctrs.verbose_materialize"));

  if (option != r_null) {
    if (r_typeof(option) == R_TYPE_logical) {
      Rprintf("sparsevctrs: Sparse vector materialized\n");
    }
    if (r_typeof(option) == R_TYPE_double) {
      if (*r_dbl_cbegin(option) == 3) {
        Rf_error("sparsevctrs: Sparse vector materialized");
      } else if (*r_dbl_cbegin(option) == 2) {
        Rf_warning("sparsevctrs: Sparse vector materialized");
      } else {
        Rprintf("sparsevctrs: Sparse vector materialized\n");
      }
    }
    if (r_typeof(option) == R_TYPE_integer) {
      if (*r_int_cbegin(option) == 3) {
        Rf_error("sparsevctrs: Sparse vector materialized");
      } else if (*r_int_cbegin(option) == 2) {
        Rf_warning("sparsevctrs: Sparse vector materialized");
      } else {
        Rprintf("sparsevctrs: Sparse vector materialized\n");
      }
    }
  }
}

void sparse_sort_pos_and_val(r_obj* pos, r_obj* val) {
  const r_ssize len = r_length(pos);

  // nothing to sort -> stop early
  if (len < 2) {
    return;
  }

  int* v_pos = r_int_begin(pos);

  if (pos_is_sorted(v_pos, len)) {
    return;
  }

  r_obj* index = KEEP(r_alloc_integer(len));
  int* v_index = r_int_begin(index);
  r_obj* buffer = KEEP(r_alloc_integer(len));
  int* v_buffer = r_int_begin(buffer);

  for (r_ssize i = 0; i < len; i++) {
    v_index[i] = i;
  }

  const int* v_order = merge_sort_index(v_pos, v_index, v_buffer, len);

  for (r_ssize i = 0; i < len; i++) {
    v_index[i] = v_order[i];
  }

  for (r_ssize i = 0; i < len; i++) {
    v_buffer[i] = v_pos[v_index[i]];
  }

  for (r_ssize i = 0; i < len; i++) {
    v_pos[i] = v_buffer[i];
  }

  if (r_typeof(val) == R_TYPE_integer) {
    int* v_val = r_int_begin(val);

    r_obj* sorted_val = KEEP(r_alloc_integer(len));
    int* v_sorted_val = r_int_begin(sorted_val);

    for (r_ssize i = 0; i < len; i++) {
      const int cur_index = v_index[i];

      v_sorted_val[i] = v_val[cur_index];
    }

    for (r_ssize i = 0; i < len; i++) {
      v_val[i] = v_sorted_val[i];
    }
  } else {
    double* v_val = r_dbl_begin(val);

    r_obj* sorted_val = KEEP(r_alloc_double(len));
    double* v_sorted_val = r_dbl_begin(sorted_val);

    for (r_ssize i = 0; i < len; i++) {
      const int cur_index = v_index[i];

      v_sorted_val[i] = v_val[cur_index];
    }

    for (r_ssize i = 0; i < len; i++) {
      v_val[i] = v_sorted_val[i];
    }
  }
  FREE(3);
}

static bool pos_is_sorted(const int* v_pos, r_ssize len) {
  for (r_ssize i = 1; i < len; i++) {
    if (v_pos[i - 1] > v_pos[i]) {
      return false;
    }
  }
  return true;
}

static const int* merge_sort_index(
  const int* v_pos,
  int* v_a,
  int* v_b,
  r_ssize len
) {
  const int* v_src = v_a;
  int* v_dst = v_b;

  for (r_ssize width = 1; width < len; width *= 2) {
    for (r_ssize lo = 0; lo < len; lo += 2 * width) {
      const r_ssize mid = lo + width < len ? lo + width : len;
      const r_ssize hi = lo + 2 * width < len ? lo + 2 * width : len;

      r_ssize i = lo;
      r_ssize j = mid;
      r_ssize k = lo;

      while (i < mid && j < hi) {
        if (v_pos[v_src[j]] < v_pos[v_src[i]]) {
          v_dst[k++] = v_src[j++];
        } else {
          v_dst[k++] = v_src[i++];
        }
      }
      while (i < mid) {
        v_dst[k++] = v_src[i++];
      }
      while (j < hi) {
        v_dst[k++] = v_src[j++];
      }
    }

    const int* v_done = v_dst;
    v_dst = v_dst == v_a ? v_b : v_a;
    v_src = v_done;
  }

  return v_src;
}

int sparse_ssize_as_int(r_ssize x) {
  if (x > INT_MAX) {
    Rf_error("Can't convert a length of %td to an integer.", (ptrdiff_t) x);
  }

  return (int) x;
}
