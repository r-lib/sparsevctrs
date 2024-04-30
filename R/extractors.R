.positions <- function(x) {
  if (!is_sparse_vector(x)) {
    return(seq_along(x))
  }

  .Call(ffi_altrep_sparse_positions, x)
}

.values <- function(x) {
  if (!is_sparse_vector(x)) {
    return(x)
  }

  .Call(ffi_altrep_sparse_values, x)
}
