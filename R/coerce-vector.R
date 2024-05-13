#' Coerce numeric vector to sparse double
#' 
#' Takes a numeric vector, integer or double, and turn it into a sparse double
#' vector.
#' 
#' @param x a numeric vector.
#' @param default default value to use. Defaults to `0`.
#' 
#' The values of `x` must be double or integer. It must not contain any `Inf` or
#' `NaN` values.
#'
#' @examples
#' x_dense <- c(3, 0, 2, 0, 0, 0, 4, 0, 0, 0)
#' x_sparse <- as_sparse_double(x_dense)
#' x_sparse
#' 
#' is_sparse_double(x_sparse) 
#' @export
as_sparse_double <- function(x, default = 0) {
  if (is_sparse_double(x)) {
    return(x)
  }

  validate_values_double(x)

  check_number_decimal(default)

  index <- which(x != default | is.na(x))

  sparse_double(
    values = x[index],
    positions = index,
    length = length(x),
    default = default
  )
}