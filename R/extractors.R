.positions <- function(x) {
  .Call(ffi_altrep_sparse_positions, x)
}

.values <- function(x) {
  .Call(ffi_altrep_sparse_values, x)
}
