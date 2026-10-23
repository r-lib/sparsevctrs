static SEXP multiplication_integers(SEXP x, SEXP y);

static SEXP multiplication_integers_sparse_sparse(SEXP x, SEXP y);

static SEXP multiplication_integers_sparse_dense(SEXP x, SEXP y);

static SEXP multiplication_integers_dense_dense(SEXP x, SEXP y);

static SEXP multiplication_doubles(SEXP x, SEXP y);

static SEXP multiplication_doubles_sparse_sparse(SEXP x, SEXP y);

static SEXP multiplication_doubles_sparse_dense(SEXP x, SEXP y);

static SEXP multiplication_doubles_dense_dense(SEXP x, SEXP y);

static SEXP find_overlap(SEXP x, SEXP y);

static SEXP find_nas_with_no_overlap(SEXP x, SEXP y);

static Rboolean int_match(int x, SEXP table);

static SEXP empty_sparse_integer(R_xlen_t len);

static SEXP empty_sparse_double(R_xlen_t len);
