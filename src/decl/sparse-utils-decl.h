static inline r_obj* altrep_package(r_obj* x);

static inline r_ssize midpoint(r_ssize lhs, r_ssize rhs);

static bool pos_is_sorted(const int* v_pos, r_ssize len);

static const int* merge_sort_index(
  const int* v_pos,
  int* v_a,
  int* v_b,
  r_ssize len
);
