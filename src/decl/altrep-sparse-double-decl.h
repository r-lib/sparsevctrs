static SEXP altrep_sparse_double_Materialize(SEXP x);

static void* altrep_sparse_double_Dataptr(SEXP x, Rboolean writeable);

static const void* altrep_sparse_double_Dataptr_or_null(SEXP x);

static SEXP altrep_sparse_double_Extract_subset(SEXP x, SEXP indx, SEXP call);

static R_xlen_t altrep_sparse_double_Length(SEXP x);

static Rboolean altrep_sparse_double_Inspect(
  SEXP x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(SEXP, int, int, int)
);

static SEXP altrep_sparse_double_Duplicate(SEXP x, Rboolean deep);

static double altrep_sparse_double_Elt(SEXP x, R_xlen_t i);

static int altrep_sparse_double_Is_sorted(SEXP x);

static SEXP altrep_sparse_double_Min_method(SEXP x, Rboolean na_rm);

static SEXP altrep_sparse_double_Max_method(SEXP x, Rboolean na_rm);

static int altrep_sparse_double_No_NA_method(SEXP x);

static SEXP altrep_sparse_double_Sum_method(SEXP x, Rboolean na_rm);
