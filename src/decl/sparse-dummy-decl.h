static void sparse_dummy_count(
  const int* v_x,
  r_ssize len,
  unsigned int shift,
  r_ssize n_lvls,
  int* v_counts
);

static r_obj* sparse_dummy_no_na(
  const int* v_x,
  r_ssize len,
  unsigned int shift,
  r_ssize n_lvls,
  const int* v_counts,
  r_obj* out_len,
  r_obj* out_default
);

static r_obj* sparse_dummy_na(
  const int* v_x,
  r_ssize len,
  unsigned int shift,
  r_ssize n_lvls,
  const int* v_counts,
  r_ssize n_na,
  r_obj* out_len,
  r_obj* out_default
);

static inline void sparse_dummy_fill_na(
  int* v_pos,
  int* v_val,
  const int* v_na_pos,
  r_ssize n
);

static r_obj* create_dummy(r_obj* val, r_obj* pos, r_obj* len, r_obj* def);
