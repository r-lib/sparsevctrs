#' Sparse vector type checkers
#' 
#' @param x value to be checked.
#'
#' @examples
#' x_sparse <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)
#' x_dense <- c(0, pi, 0, 0, 0.5, 0, 0, 0, 0, 0.1)
#' 
#' is_sparse_vector(x_sparse)
#' is_sparse_vector(x_dense)
#' 
#' # Forced materialization
#' is_sparse_vector(x_sparse[])
#' @name type-predicates
NULL

#' @rdname type-predicates
#' @export
is_sparse_vector <- function(x) {
  res <- .Call(ffi_extract_altrep_class, x)
  if (is.null(res)) {
    return(FALSE)
  }
  
  res <- as.character(res[[1]])

  res %in% c("altrep_sparse_double")
}
 
#' @rdname type-predicates
#' @export
is_sparse_double <- function(x) {
  res <- .Call(ffi_extract_altrep_class, x)
  if (is.null(res)) {
    return(FALSE)
  }
  
  res <- as.character(res[[1]])

  res == "altrep_sparse_double"
}
