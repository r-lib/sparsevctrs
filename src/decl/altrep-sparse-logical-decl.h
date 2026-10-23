static SEXP altrep_sparse_logical_Materialize(SEXP x);

static void* altrep_sparse_logical_Dataptr(SEXP x, Rboolean writeable);

static const void* altrep_sparse_logical_Dataptr_or_null(SEXP x);

static SEXP altrep_sparse_logical_Extract_subset(SEXP x, SEXP indx, SEXP call);

static R_xlen_t altrep_sparse_logical_Length(SEXP x);

static Rboolean altrep_sparse_logical_Inspect(
  SEXP x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(SEXP, int, int, int)
);

static SEXP altrep_sparse_logical_Duplicate(SEXP x, Rboolean deep);

static int altrep_sparse_logical_Elt(SEXP x, R_xlen_t i);

static int altrep_sparse_logical_Is_sorted(SEXP x);

static int altrep_sparse_logical_No_NA_method(SEXP x);

static SEXP altrep_sparse_logical_Sum_method(SEXP x, Rboolean na_rm);
