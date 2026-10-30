#' Scalar arithmetic with sparse vectors
#'
#' Do Arithmetic on sparse vectors without destroying the sparsity. Note that
#' only multiplication and division preserves the default value.
#'
#' @param x A sparse vector.
#' @param val A single numeric value.
#'
#' @details
#' Only minimal checking of the inputs is done, for performance reasons.
#'
#' `sparse_division_scalar()` and `sparse_multiplication_scalar()` are the most
#' used ones, as they preserve the default, which is often what you want to do.
#'
#' `sparse_division_scalar()` always produces double vectors, regardless of
#' whether they could be represented as integers or not. Expect when `val = 1`
#' as the input is returned unchanged, or `val = NA` as the input returned will
#' be `NA` or the appropiate type. When `val = 0` the result contains `Inf`,
#' `-Inf` and `NaN`, which sparse vectors can't store, so a regular (dense)
#' double vector is returned.
#'
#' The other functions return a sparse vector of the same type as `x`. This
#' means that when `x` is a sparse integer vector, `val` must give whole number
#' results. Integer overflow in the stored values produces `NA` with a warning,
#' as it does for dense integer vectors.
#'
#' Results that sparse double vectors can't store, such as stored values that
#' are `Inf` or `NaN`, or a default value that is `NaN`, give an error.
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
#' @name sparse-arithmetic-scalar
#' @aliases sparse-arithmatic-scalar
NULL

#' @rdname sparse-arithmetic-scalar
#' @export
sparse_division_scalar <- function(x, val) {
  if (is.na(val)) {
    return(scalar_na(x))
  }

  if (val == 0) {
    res <- rep(sparse_default(x) / val, length(x))
    res[sparse_positions(x)] <- sparse_values(x) / val
    return(res)
  }

  if (val == 1) {
    return(x)
  }

  check_sparse_numeric(x)

  .Call(ffi_sparse_division_scalar, x, val)
}

#' @rdname sparse-arithmetic-scalar
#' @export
sparse_multiplication_scalar <- function(x, val) {
  if (is.na(val)) {
    return(scalar_na(x))
  }

  if (val == 1) {
    return(x)
  }

  check_sparse_numeric(x)

  .Call(ffi_sparse_multiplication_scalar, x, val)
}

#' @rdname sparse-arithmetic-scalar
#' @export
sparse_addition_scalar <- function(x, val) {
  if (is.na(val)) {
    return(scalar_na(x))
  }

  if (val == 0) {
    return(x)
  }

  check_sparse_numeric(x)

  .Call(ffi_sparse_addition_scalar, x, val)
}

#' @rdname sparse-arithmetic-scalar
#' @export
sparse_subtraction_scalar <- function(x, val) {
  if (is.na(val)) {
    return(scalar_na(x))
  }

  if (val == 0) {
    return(x)
  }

  check_sparse_numeric(x)

  .Call(ffi_sparse_subtraction_scalar, x, val)
}

scalar_na <- function(x) {
  if (is.integer(x)) {
    rep(NA_integer_, length(x))
  } else {
    rep(NA_real_, length(x))
  }
}

check_sparse_numeric <- function(x, call = rlang::caller_env()) {
  if (!is_sparse_numeric(x)) {
    cli::cli_abort(
      "{.arg x} must be a sparse integer or double vector.",
      call = call
    )
  }
}

#' Vector arithmetic with sparse vectors
#'
#' Do arithmetic operations on sparse vectors while trying to avoid destroying
#' the sparsity.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#'
#' @details
#'
#' Note that this function works with both sparse and dense vectors for both `x`
#' and `y`, returning a sparse or dense vector according to the input.
#'
#' For `sparse_multiplication()` the class of the resulting vector depends on
#' the classes of `x` and `y`. If both `x` and `y` are integer vectors then an
#' integer vector is returned, otherwise a double vector is returned.
#'
#' `sparse_multiplication()` will return a non-sparse vector if both `x` and `y`
#' is non-sparse. Otherwise a sparse vector is returned.
#'
#' `sparse_multiplication()` will destroy sparsity of sparse vectors with non-0
#' `default` values.
#'
#' @return A sparse vector of same type.
#'
#' @examples
#' x_sparse <- sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)
#'
#' sparse_multiplication(x_sparse, x_sparse)
#' @name sparse-arithmetic
#' @aliases sparse-arithmatic
NULL

#' @rdname sparse-arithmetic
#' @export
sparse_multiplication <- function(x, y) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be numeric, not {.obj_type_friendly {x}}.")
  }
  if (!is.numeric(y)) {
    cli::cli_abort("{.arg y} must be numeric, not {.obj_type_friendly {y}}.")
  }

  if (length(x) != length(y)) {
    x_len <- length(x)
    y_len <- length(y)
    cli::cli_abort(
      "{.arg x} ({x_len}) and {.arg y} ({y_len}) must be the same length."
    )
  }

  x_class <- class(x)
  y_class <- class(y)

  if (x_class != y_class) {
    if (x_class == "integer") {
      if (is_sparse_vector(x)) {
        x <- as_sparse_double(x, default = sparse_default(x))
      } else {
        x <- as.double(x)
      }
    } else {
      if (is_sparse_vector(y)) {
        y <- as_sparse_double(y, default = sparse_default(y))
      } else {
        y <- as.double(y)
      }
    }
  }

  x_default <- sparse_default(x)
  y_default <- sparse_default(y)

  if (!is.na(x_default) && x_default != 0) {
    x <- x[]
  }

  if (!is.na(y_default) && y_default != 0) {
    y <- y[]
  }

  .Call(ffi_sparse_multiplication, x, y)
}
