#' Create sparse numeric vector
#' 
#' @param value Numeric vector, values of non-zero entries.
#' @param position integer vector, indices of non-zero entries.
#' @param length Integer, Length of vector.
#'
#' @export
new_sparse_real <- function(value, position, length) {
  x <- list(val = value, pos = as.integer(position), length = as.integer(length))

  .Call(ffi_altrep_new_sparse_real, x)
}
