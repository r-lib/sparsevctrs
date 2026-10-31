#include "sparsevctrs-core.h"

#include "sparse-arithmetic-scalar.h"
#include "sparse-utils.h"
#include "altrep-sparse-double.h"
#include "altrep-sparse-integer.h"

#include "decl/sparse-arithmetic-scalar-decl.h"

r_obj* ffi_sparse_addition_scalar(r_obj* ffi_x, r_obj* ffi_val) {
  return sparse_scalar_arithmetic(ffi_x, ffi_val, SPARSE_SCALAR_OP_add);
}

r_obj* ffi_sparse_subtraction_scalar(r_obj* ffi_x, r_obj* ffi_val) {
  return sparse_scalar_arithmetic(ffi_x, ffi_val, SPARSE_SCALAR_OP_sub);
}

r_obj* ffi_sparse_multiplication_scalar(r_obj* ffi_x, r_obj* ffi_val) {
  return sparse_scalar_arithmetic(ffi_x, ffi_val, SPARSE_SCALAR_OP_mul);
}

r_obj* ffi_sparse_division_scalar(r_obj* ffi_x, r_obj* ffi_val) {
  return sparse_scalar_arithmetic(ffi_x, ffi_val, SPARSE_SCALAR_OP_div);
}

static r_obj* sparse_scalar_arithmetic(
  r_obj* x,
  r_obj* ffi_val,
  enum sparse_scalar_op op
) {
  const double val = sparse_scalar_val(ffi_val);

  if (r_typeof(x) == R_TYPE_integer && op != SPARSE_SCALAR_OP_div) {
    return sparse_scalar_integer(x, val, op);
  }

  return sparse_scalar_double(x, val, op);
}

static double sparse_scalar_val(r_obj* ffi_val) {
  if (r_length(ffi_val) != 1) {
    Rf_error("`val` must be a single number.");
  }

  switch (r_typeof(ffi_val)) {
  case R_TYPE_double: {
    return r_dbl_get(ffi_val, 0);
  }
  case R_TYPE_integer: {
    const int val = r_int_get(ffi_val, 0);
    return val == r_globals.na_int ? r_globals.na_dbl : (double) val;
  }
  case R_TYPE_logical: {
    const int val = r_lgl_get(ffi_val, 0);
    return val == r_globals.na_lgl ? r_globals.na_dbl : (double) val;
  }
  default: {
    Rf_error("`val` must be a single number.");
  }
  }
}

static r_obj* sparse_scalar_integer(
  r_obj* x,
  double val,
  enum sparse_scalar_op op
) {
  const int out_default_val =
    sparse_scalar_int_op(sparse_extract_default_integer(x), val, op);
  if (out_default_val == r_globals.na_int) {
    Rf_error("Integer overflow in the default value of `x`.");
  }

  const bool keeps_only_na = op == SPARSE_SCALAR_OP_mul && val == 0;

  r_obj* out_pos = KEEP(
    keeps_only_na ? sparse_scalar_non_finite_pos(x) : sparse_extract_pos(x)
  );
  const r_ssize n_pos = r_length(out_pos);

  r_obj* out_val = KEEP(r_alloc_integer(n_pos));
  int* v_out_val = r_int_begin(out_val);

  bool has_overflow = false;

  if (keeps_only_na) {
    for (r_ssize i = 0; i < n_pos; ++i) {
      v_out_val[i] = r_globals.na_int;
    }
  } else {
    const int* v_val = r_int_cbegin(sparse_extract_val(x));

    for (r_ssize i = 0; i < n_pos; ++i) {
      const int elt = v_val[i];
      const int res = sparse_scalar_int_op(elt, val, op);

      has_overflow |= (res == r_globals.na_int) && (elt != r_globals.na_int);
      v_out_val[i] = res;
    }
  }

  if (has_overflow) {
    Rf_warning("NAs produced by integer overflow");
  }

  r_obj* out_len = KEEP(r_int((int) sparse_extract_len(x)));
  r_obj* out_default = KEEP(r_int(out_default_val));

  r_obj* out = new_sparse_integer(out_val, out_pos, out_len, out_default);

  FREE(4);

  return out;
}

static r_obj* sparse_scalar_double(
  r_obj* x,
  double val,
  enum sparse_scalar_op op
) {
  const bool is_integer = r_typeof(x) == R_TYPE_integer;

  const double default_val = is_integer
    ? (double) sparse_extract_default_integer(x)
    : sparse_extract_default_double(x);
  const double out_default_val = sparse_scalar_dbl_op(default_val, val, op);
  if (ISNAN(out_default_val)) {
    Rf_error(
      "The result has a `NaN` default value, which sparse double vectors can't "
      "store."
    );
  }

  const bool keeps_only_na = op == SPARSE_SCALAR_OP_mul && val == 0;

  r_obj* out_pos = KEEP(
    keeps_only_na ? sparse_scalar_non_finite_pos(x) : sparse_extract_pos(x)
  );
  const r_ssize n_pos = r_length(out_pos);

  r_obj* out_val = KEEP(r_alloc_double(n_pos));
  double* v_out_val = r_dbl_begin(out_val);

  r_obj* x_val = sparse_extract_val(x);

  bool has_non_finite = false;

  if (keeps_only_na) {
    const double* v_val = r_dbl_cbegin(x_val);
    const r_ssize n_val = r_length(x_val);

    r_ssize idx = 0;
    for (r_ssize i = 0; i < n_val; ++i) {
      const double elt = v_val[i];

      if (!R_FINITE(elt)) {
        v_out_val[idx] = R_IsNA(elt) ? r_globals.na_dbl : 0 * elt;
        ++idx;
      }
    }
  } else if (is_integer) {
    const int* v_val = r_int_cbegin(x_val);

    for (r_ssize i = 0; i < n_pos; ++i) {
      const int elt = v_val[i];
      const double res = elt == r_globals.na_int
        ? r_globals.na_dbl
        : sparse_scalar_dbl_op((double) elt, val, op);

      has_non_finite |= !(R_FINITE(res) || R_IsNA(res));
      v_out_val[i] = res;
    }
  } else {
    const double* v_val = r_dbl_cbegin(x_val);

    for (r_ssize i = 0; i < n_pos; ++i) {
      const double res = sparse_scalar_dbl_op(v_val[i], val, op);

      has_non_finite |= !(R_FINITE(res) || R_IsNA(res));
      v_out_val[i] = res;
    }
  }

  if (has_non_finite) {
    Rf_error(
      "The result has `Inf` or `NaN` values, which sparse double vectors can't "
      "store."
    );
  }

  r_obj* out_len = KEEP(r_int((int) sparse_extract_len(x)));
  r_obj* out_default = KEEP(r_dbl(out_default_val));

  r_obj* out = new_sparse_double(out_val, out_pos, out_len, out_default);

  FREE(4);

  return out;
}

static r_obj* sparse_scalar_non_finite_pos(r_obj* x) {
  r_obj* x_val = sparse_extract_val(x);
  const int* v_x_pos = r_int_cbegin(sparse_extract_pos(x));
  const r_ssize n_val = r_length(x_val);

  r_obj* out_pos = KEEP(r_alloc_integer(n_val));
  int* v_out_pos = r_int_begin(out_pos);

  r_ssize n_na = 0;

  if (r_typeof(x_val) == R_TYPE_integer) {
    const int* v_x_val = r_int_cbegin(x_val);

    for (r_ssize i = 0; i < n_val; ++i) {
      v_out_pos[n_na] = v_x_pos[i];
      n_na += v_x_val[i] == r_globals.na_int;
    }
  } else {
    const double* v_x_val = r_dbl_cbegin(x_val);

    for (r_ssize i = 0; i < n_val; ++i) {
      v_out_pos[n_na] = v_x_pos[i];
      n_na += !R_FINITE(v_x_val[i]);
    }
  }

  r_obj* out = r_int_resize(out_pos, n_na);

  FREE(1);

  return out;
}

static inline int sparse_scalar_int_op(
  int x,
  double val,
  enum sparse_scalar_op op
) {
  if (x == r_globals.na_int) {
    return r_globals.na_int;
  }

  const double out = sparse_scalar_dbl_op((double) x, val, op);

  if (out != floor(out)) {
    Rf_error(
      "`val` must give whole number results when `x` is a sparse integer "
      "vector."
    );
  }

  if (out > INT_MAX || out < -INT_MAX) {
    return r_globals.na_int;
  }

  return (int) out;
}

static inline double sparse_scalar_dbl_op(
  double x,
  double val,
  enum sparse_scalar_op op
) {
  switch (op) {
  case SPARSE_SCALAR_OP_add: {
    return x + val;
  }
  case SPARSE_SCALAR_OP_sub: {
    return x - val;
  }
  case SPARSE_SCALAR_OP_mul: {
    return x * val;
  }
  case SPARSE_SCALAR_OP_div: {
    return x / val;
  }
  }

  r_stop_unreachable();
}
