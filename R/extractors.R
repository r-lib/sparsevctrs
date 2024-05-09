#' Information extraction from sparse vectors
#' 
#' Extract positions and values from sparse vectors without the need to 
#' materialize vector.
#' 
#' @param x vector to be extracted from.
#'
#' @details
#' for ease of use, these functions also works on non-sparse variables.
#' 
#' @examples
#' x_sparse <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)
#' x_dense <- c(0, pi, 0, 0, 0.5, 0, 0, 0, 0, 0.1)
#' 
#' sparse_positions(x_sparse)
#' sparse_values(x_sparse)
#' 
#' sparse_positions(x_dense)
#' sparse_values(x_dense)
#' @name extractors
NULL

#' @rdname extractors
#' @export
sparse_positions <- function(x) {
  if (!is_sparse_vector(x)) {
    return(seq_along(x))
  }

  .Call(ffi_altrep_sparse_positions, x)
}

#' @rdname extractors
#' @export
sparse_values <- function(x) {
  if (!is_sparse_vector(x)) {
    return(x)
  }

  .Call(ffi_altrep_sparse_values, x)
}
