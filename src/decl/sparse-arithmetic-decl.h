static r_obj* multiplication(r_obj* x, r_obj* y);

static r_obj* multiplication_sparse_sparse(r_obj* x, r_obj* y);

static r_obj* multiplication_sparse_dense(r_obj* x, r_obj* y);

static r_obj* multiplication_dense_dense(r_obj* x, r_obj* y);

static inline bool is_na_at(const void* v, r_ssize i, bool is_integer);

static inline bool is_zero_at(const void* v, r_ssize i, bool is_integer);

static inline void poke_na(void* v_out, r_ssize i_out, bool is_integer);

static inline void poke_product(
  void* v_out,
  r_ssize i_out,
  const void* v_x,
  r_ssize i_x,
  const void* v_y,
  r_ssize i_y,
  bool is_integer
);

static r_obj* alloc_vec(bool is_integer, r_ssize n);

static r_obj* find_overlap(r_obj* x, r_obj* y);

static r_obj* find_nas_with_no_overlap(r_obj* x, r_obj* y);

static bool int_match(int x, r_obj* table);

static void mark_nas(r_obj* val, int* v_matches);

static r_obj* new_sparse_product(r_obj* val, r_obj* pos, r_ssize len);

static r_obj* empty_sparse(r_ssize len, bool is_integer);
