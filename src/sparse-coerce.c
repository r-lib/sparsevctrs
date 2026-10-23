#include "sparsevctrs-core.h"

#include "altrep-sparse-double.h"

r_obj* ffi_sparse_matrix_to_list(
  r_obj* ffi_i,
  r_obj* ffi_p,
  r_obj* ffi_x,
  r_obj* ffi_nrow
) {
  const int* v_i = r_int_cbegin(ffi_i);
  const int* v_p = r_int_cbegin(ffi_p);
  const double* v_x = ffi_x == r_null ? NULL : r_dbl_cbegin(ffi_x);

  const r_ssize n_col = r_length(ffi_p) - 1;

  r_obj* out = KEEP(r_alloc_list(n_col));

  // Shared between all columns
  r_obj* len = KEEP(r_int(r_int_get(ffi_nrow, 0)));
  r_obj* def = KEEP(r_dbl(0));
  r_mark_shared(len);
  r_mark_shared(def);

  for (r_ssize col = 0; col < n_col; ++col) {
    const int start = v_p[col];
    const r_ssize n = v_p[col + 1] - start;

    r_obj* val = KEEP(r_alloc_double(n));
    double* v_val = r_dbl_begin(val);

    r_obj* pos = KEEP(r_alloc_integer(n));
    int* v_pos = r_int_begin(pos);

    for (r_ssize j = 0; j < n; ++j) {
      v_val[j] = v_x == NULL ? 1 : v_x[start + j];
      v_pos[j] = v_i[start + j] + 1;
    }

    r_list_poke(out, col, new_sparse_double(val, pos, len, def));
    FREE(2);
  }

  FREE(3);
  return out;
}
