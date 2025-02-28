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
#' @return sparse vectors
#'
#' @examples
#' x_dense <- c(3, 0, 2, 0, 0, 0, 4, 0, 0, 0)
#' x_sparse <- as_sparse_double(x_dense)
#' x_sparse
#'
#' is_sparse_double(x_sparse)
#' @name coerce-vector
NULL

#' @rdname coerce-vector
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

#' @rdname coerce-vector
#' @export
as_sparse_integer <- function(x, default = 0L) {
  if (is_sparse_integer(x)) {
    return(x)
  }

  validate_values_integer(x)
  check_number_whole(default)

  values <- vctrs::vec_cast(x, integer())
  default <- vctrs::vec_cast(default, integer())

  index <- which(x != default | is.na(x))

  sparse_integer(
    values = x[index],
    positions = index,
    length = length(x),
    default = default
  )
}

#' @rdname coerce-vector
#' @export
as_sparse_character <- function(x, default = "") {
  if (is_sparse_character(x)) {
    return(x)
  }

  check_string(default)

  values <- vctrs::vec_cast(x, character())
  default <- vctrs::vec_cast(default, character())

  index <- which(x != default | is.na(x))

  sparse_character(
    values = x[index],
    positions = index,
    length = length(x),
    default = default
  )
}

#' @rdname coerce-vector
#' @export
as_sparse_logical <- function(x, default = FALSE) {
  if (is_sparse_logical(x)) {
    return(x)
  }

  check_bool(default)

  index <- which(x != default | is.na(x))

  sparse_logical(
    values = x[index],
    positions = index,
    length = length(x),
    default = default
  )
}
