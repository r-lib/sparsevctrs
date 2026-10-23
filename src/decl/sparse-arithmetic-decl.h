static r_obj* multiplication_integers(r_obj* x, r_obj* y);

static r_obj* multiplication_integers_sparse_sparse(r_obj* x, r_obj* y);

static r_obj* multiplication_integers_sparse_dense(r_obj* x, r_obj* y);

static r_obj* multiplication_integers_dense_dense(r_obj* x, r_obj* y);

static r_obj* multiplication_doubles(r_obj* x, r_obj* y);

static r_obj* multiplication_doubles_sparse_sparse(r_obj* x, r_obj* y);

static r_obj* multiplication_doubles_sparse_dense(r_obj* x, r_obj* y);

static r_obj* multiplication_doubles_dense_dense(r_obj* x, r_obj* y);

static r_obj* find_overlap(r_obj* x, r_obj* y);

static r_obj* find_nas_with_no_overlap(r_obj* x, r_obj* y);

static bool int_match(int x, r_obj* table);

static r_obj* empty_sparse_integer(r_ssize len);

static r_obj* empty_sparse_double(r_ssize len);
