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
#' whether they could be represented as integers or not. Expect when `val = 1`
#' as the input is returned unchanged, or `val = NA` as the input returned will
#' be `NA` or the appropiate type.
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
  if (is.na(val)) {
    if (is.integer(x)) {
      return(rep(NA_integer_, length(x)))
    } else {
      return(rep(NA_real_, length(x)))
    }
  }

  if (val == 0) {
    return(rep(Inf, length(x)))
  }

  if (val == 1) {
    return(x)
  }

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
  if (is.na(val)) {
    if (is.integer(x)) {
      return(rep(NA_integer_, length(x)))
    } else {
      return(rep(NA_real_, length(x)))
    }
  }

  if (val == 1) {
    return(x)
  }

  if (is_sparse_integer(x)) {
    if (val == 0) {
      res <- sparse_integer(
        values = integer(),
        positions = integer(),
        length = length(x),
        default = sparse_default(x)
      )
    } else {
      res <- sparse_integer(
        values = sparse_values(x) * val,
        positions = sparse_positions(x),
        length = length(x),
        default = sparse_default(x)
      )
    }
  }
  if (is_sparse_double(x)) {
    if (val == 0) {
      res <- sparse_double(
        values = double(),
        positions = integer(),
        length = length(x),
        default = sparse_default(x)
      )
    } else {
      res <- sparse_double(
        values = sparse_values(x) * val,
        positions = sparse_positions(x),
        length = length(x),
        default = sparse_default(x)
      )
    }
  }

  res
}

#' @rdname sparse-arithmatic-scalar
#' @export
sparse_addition_scalar <- function(x, val) {
  if (is.na(val)) {
    if (is.integer(x)) {
      return(rep(NA_integer_, length(x)))
    } else {
      return(rep(NA_real_, length(x)))
    }
  }

  if (val == 0) {
    return(x)
  }

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
  if (is.na(val)) {
    if (is.integer(x)) {
      return(rep(NA_integer_, length(x)))
    } else {
      return(rep(NA_real_, length(x)))
    }
  }

  if (val == 0) {
    return(x)
  }

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

#' Vector arithmatic with sparse vectors
#'
#' Do arithmatic operations on sparse vectors while trying to void destroying
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
#' @name sparse-arithmatic
NULL

#' @rdname sparse-arithmatic
#' @export
sparse_multiplication <- function(x, y) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must me numeric, not {.obj_type_friendly {x}}.")
  }
  if (!is.numeric(y)) {
    cli::cli_abort("{.arg y} must me numeric not {.obj_type_friendly {x}}.")
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

  if (is_altrep_non_sparse_vector(x) || (!is.na(x_default) && x_default != 0)) {
    x <- x[]
  }

  if (is_altrep_non_sparse_vector(y) || (!is.na(y_default) && y_default != 0)) {
    y <- y[]
  }

  .Call(ffi_sparse_multiplication, x, y)
}
