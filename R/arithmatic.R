#' Scalar arithmatic with sparse vectors
#'
#' Do Arithmatic on sparse vectors without destroying the sparsity. Note that
#' only multiplication and division preserves the default value.
#'
#' @param x A sparse vector.
#' @param val A single numeric value.
#'
#' @details
#' No checking of the inputs are being done.
#'
#' `sparse_division_scalar()` and `sparse_multiplication_scalar()` are the most
#' used ones, as they preserve the default, which is often what you want to do.
#'
#' `sparse_division_scalar()` always produces double vectors, regardless of 
#' whether they could be represented as integers or not.
#'
#' @return A sparse vector of same type.
#'
#' @examples
#' x_sparse <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)
#'
#' sparse_division_scalar(x_sparse, 2)
#' sparse_multiplication_scalar(x_sparse, 2)
#' sparse_addition_scalar(x_sparse, 2)
#' sparse_subtraction_scalar(x_sparse, 2)
#' @name sparse-arithmatic-scalar
NULL

#' @rdname sparse-arithmatic-scalar
#' @export
sparse_division_scalar <- function(x, val) {
  res <- sparse_double(
    values = sparse_values(x) / val,
    positions = sparse_positions(x),
    length = length(x),
    default = sparse_default(x)
  )

  res
}

#' @rdname sparse-arithmatic-scalar
#' @export
sparse_multiplication_scalar <- function(x, val) {
  if (is_sparse_integer(x)) {
    res <- sparse_integer(
      values = sparse_values(x) * val,
      positions = sparse_positions(x),
      length = length(x),
      default = sparse_default(x)
    )
  }
  if (is_sparse_double(x)) {
    res <- sparse_double(
      values = sparse_values(x) * val,
      positions = sparse_positions(x),
      length = length(x),
      default = sparse_default(x)
    )
  }

  res
}

#' @rdname sparse-arithmatic-scalar
#' @export
sparse_addition_scalar <- function(x, val) {
  if (is_sparse_integer(x)) {
    res <- sparse_integer(
      values = sparse_values(x) + val,
      positions = sparse_positions(x),
      length = length(x),
      default = sparse_default(x) + val
    )
  }
  if (is_sparse_double(x)) {
    res <- sparse_double(
      values = sparse_values(x) + val,
      positions = sparse_positions(x),
      length = length(x),
      default = sparse_default(x) + val
    )
  }

  res
}

#' @rdname sparse-arithmatic-scalar
#' @export
sparse_subtraction_scalar <- function(x, val) {
  if (is_sparse_integer(x)) {
    res <- sparse_integer(
      values = sparse_values(x) - val,
      positions = sparse_positions(x),
      length = length(x),
      default = sparse_default(x) - val
    )
  }
  if (is_sparse_double(x)) {
    res <- sparse_double(
      values = sparse_values(x) - val,
      positions = sparse_positions(x),
      length = length(x),
      default = sparse_default(x) - val
    )
  }

  res
}
