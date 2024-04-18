#' Create sparse numeric vector
#' 
#' @param x thing
#'
#' @export
new_sparse_real <- function(x) {
  .Call(ffi_altrep_new_sparse_real, x)
}
