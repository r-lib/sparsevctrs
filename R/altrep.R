#' Create sparse numeric vector
#' 
#' @param x thing
#' @param len length of vector
#' 
#'
#' @export
new_sparse_real <- function(x, len) {
  .Call(ffi_altrep_new_sparse_real, x, len)
}
