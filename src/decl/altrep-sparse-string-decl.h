static SEXP altrep_sparse_string_Materialize(SEXP x);

static void* altrep_sparse_string_Dataptr(SEXP x, Rboolean writeable);

static const void* altrep_sparse_string_Dataptr_or_null(SEXP x);

static SEXP altrep_sparse_string_Extract_subset(SEXP x, SEXP indx, SEXP call);

static R_xlen_t altrep_sparse_string_Length(SEXP x);

static Rboolean altrep_sparse_string_Inspect(
  SEXP x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(SEXP, int, int, int)
);

static SEXP altrep_sparse_string_Duplicate(SEXP x, Rboolean deep);

static SEXP altrep_sparse_string_Elt(SEXP x, R_xlen_t i);

static int altrep_sparse_string_Is_sorted(SEXP x);

static int compare_strings(SEXP a, SEXP b);

static void altrep_sparse_string_Set_elt(SEXP x, R_xlen_t i, SEXP value);
