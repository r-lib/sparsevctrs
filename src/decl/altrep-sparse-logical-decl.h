static r_obj* altrep_sparse_logical_Materialize(r_obj* x);

static void* altrep_sparse_logical_Dataptr(r_obj* x, Rboolean writeable);

static const void* altrep_sparse_logical_Dataptr_or_null(r_obj* x);

static r_obj* altrep_sparse_logical_Extract_subset(r_obj* x, r_obj* indx, r_obj* call);

static r_ssize altrep_sparse_logical_Length(r_obj* x);

static Rboolean altrep_sparse_logical_Inspect(
  r_obj* x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(r_obj*, int, int, int)
);

static r_obj* altrep_sparse_logical_Duplicate(r_obj* x, Rboolean deep);

static int altrep_sparse_logical_Elt(r_obj* x, r_ssize i);

static int altrep_sparse_logical_Is_sorted(r_obj* x);

static int altrep_sparse_logical_No_NA_method(r_obj* x);

static r_obj* altrep_sparse_logical_Sum_method(r_obj* x, Rboolean na_rm);
