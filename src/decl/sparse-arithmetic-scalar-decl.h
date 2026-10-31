static r_obj* sparse_scalar_arithmetic(
  r_obj* x,
  r_obj* ffi_val,
  enum sparse_scalar_op op
);

static double sparse_scalar_val(r_obj* ffi_val);

static bool sparse_scalar_is_whole(double val);

static r_obj* sparse_scalar_integer(
  r_obj* x,
  double val,
  enum sparse_scalar_op op
);

static r_obj* sparse_scalar_double(
  r_obj* x,
  double val,
  enum sparse_scalar_op op
);

static r_obj* sparse_scalar_non_finite_pos(r_obj* x);

static inline int sparse_scalar_int_op(
  int x,
  double val,
  enum sparse_scalar_op op
);

static inline double sparse_scalar_dbl_op(
  double x,
  double val,
  enum sparse_scalar_op op
);
