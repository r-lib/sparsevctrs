static inline bool sparse_pos_is_stored(
  const int* v_pos,
  r_ssize n_pos,
  r_ssize* p_j,
  r_ssize i
);

static inline bool sparse_double_equal(double x, double y);

static inline bool sparse_string_equal(r_obj* x, r_obj* y);

static r_obj* sparse_materialized_pos(r_obj* x, r_obj* data2);

static r_ssize sparse_materialized_pos_scan(
  r_obj* x,
  r_obj* data2,
  const int* v_old_pos,
  r_ssize n_old_pos,
  int* v_out
);

static r_obj* sparse_materialized_val(r_obj* x, r_obj* data2, r_obj* pos);
