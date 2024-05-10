#' Coerce numeric vector to sparse double
#' 
#' @param x a numeric vector.
#' @param default default value to use. Defaults to `0`.
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

  if ((!is.numeric(x)) || (!is.vector(x))) {
    cli::cli_abort(
      "{.arg x} must be numeric vector, not {.obj_type_friendly {x}}."
    )
  }

  check_number_decimal(default)

  index <- which(x != default)

  sparse_double(
    values = x[index],
    positions = index,
    length = length(x),
    default = default
  )
}