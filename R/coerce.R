#' Coerce sparse data frame to sparse matrix
#'
#' Turning data frame with sparse columns into sparse matrix using
#' [Matrix::sparseMatrix()].
#'
#' @param x a data frame or tibble with sparse columns.
#' @inheritParams rlang::args_error_context
#'
#' @details
#' No checking is currently do to `x` to determine whether it contains sparse
#' columns or not. Thus it works with any data frame. Needless to say, creating
#' a sparse matrix out of a dense data frame is not ideal.
#'
#' @return sparse matrix
#'
#' @seealso [coerce_to_sparse_data_frame()] [coerce_to_sparse_tibble()]
#' @examplesIf rlang::is_installed("Matrix")
#' sparse_tbl <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
#' names(sparse_tbl) <- letters[1:10]
#' sparse_tbl <- as.data.frame(sparse_tbl)
#' sparse_tbl
#'
#' res <- coerce_to_sparse_matrix(sparse_tbl)
#' res
#' @export
coerce_to_sparse_matrix <- function(x, call = rlang::caller_env(0)) {
  rlang::check_installed("Matrix")

  if (!inherits(x, "data.frame")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls data.frame}, not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  if (!all(vapply(x, is.numeric, logical(1)))) {
    offenders <- which(!vapply(x, is.numeric, logical(1)))
    offenders <- names(x)[offenders]
    cli::cli_abort(
      c(
        x = "All columns of {.arg x} must be numeric.",
        i = "Non-numeric columns: {.field {offenders}}."
      ),
      call = call
    )
  }

  if (!any(vapply(x, is_sparse_numeric, logical(1)))) {
    res <- as.matrix(x)
    res <- Matrix::Matrix(res, sparse = TRUE)
    return(res)
  }

  if (!all(vapply(x, sparse_default, numeric(1)) == 0, na.rm = TRUE)) {
    offenders <- which(vapply(x, sparse_default, numeric(1)) != 0)

    for (i in offenders) {
      x[[i]] <- x[[i]][]
    }
  }

  all_positions <- lapply(x, sparse_positions)
  all_values <- lapply(x, sparse_values)

  all_rows <- rep(seq_along(x), times = lengths(all_positions))

  all_positions <- unlist(all_positions, use.names = FALSE)
  all_values <- unlist(all_values, use.names = FALSE)

  # TODO: maybe faster to do this above?
  non_zero <- all_values != 0 | is.na(all_values)
  all_rows <- all_rows[non_zero]
  all_positions <- all_positions[non_zero]
  all_values <- all_values[non_zero]

  n_row <- nrow(x)
  n_col <- ncol(x)

  if (identical(rownames(x), as.character(seq_len(nrow(x))))) {
    row_names <- NULL
  } else {
    row_names <- rownames(x)
  }

  res <- Matrix::sparseMatrix(
    i = all_positions,
    j = all_rows,
    x = all_values,
    dims = c(n_row, n_col),
    dimnames = list(row_names, colnames(x))
  )
  res
}

#' Coerce sparse matrix to tibble with sparse columns
#'
#' Turning a sparse matrix into a tibble.
#'
#' @param x sparse matrix.
#' @inheritParams rlang::args_error_context
#'
#' @details
#' The only requirement from the sparse matrix is that it contains column names.
#'
#' @return tibble with sparse columns
#'
#' @seealso [coerce_to_sparse_data_frame()] [coerce_to_sparse_matrix()]
#' @examplesIf rlang::is_installed("tibble")
#' set.seed(1234)
#' mat <- matrix(sample(0:1, 100, TRUE, c(0.9, 0.1)), nrow = 10)
#' colnames(mat) <- letters[1:10]
#' sparse_mat <- Matrix::Matrix(mat, sparse = TRUE)
#' sparse_mat
#'
#' res <- coerce_to_sparse_tibble(sparse_mat)
#' res
#'
#' # All columns are sparse
#' vapply(res, is_sparse_vector, logical(1))
#' @export
coerce_to_sparse_tibble <- function(x, call = rlang::caller_env(0)) {
  rlang::check_installed("tibble")

  if (!any(methods::is(x) == "sparseMatrix")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls sparseMatrix}, not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  if (!methods::is(x, "dgCMatrix")) {
    x <- methods::as(x, "generalMatrix")
    x <- methods::as(x, "CsparseMatrix")
  }

  if (is.null(colnames(x))) {
    cli::cli_abort(
      "{.arg x} must have column names.",
      call = call
    )
  }

  res <- .sparse_matrix_to_list(x)
  res <- tibble::as_tibble(res)
  res
}

#' Coerce sparse matrix or data frame to data frame with sparse columns
#'
#' Turning a sparse matrix or a data frame into a data frame with sparse
#' columns.
#'
#' @param x sparse matrix, or a data frame or tibble.
#' @inheritParams rlang::args_error_context
#'
#' @details
#' The only requirement from a sparse matrix is that it contains column names.
#'
#' When `x` is a data frame, each column is turned into a sparse vector of the
#' same type using [as_sparse_integer()], [as_sparse_double()],
#' [as_sparse_logical()] or [as_sparse_character()], with the default value of
#' each function. Columns that are already sparse are left as is. All columns
#' must be integer, double, logical or character vectors. Column names and row
#' names are kept, and the result is a plain data frame even if `x` is a tibble.
#'
#' @return data.frame with sparse columns
#'
#' @seealso [coerce_to_sparse_tibble()] [coerce_to_sparse_matrix()]
#' @examplesIf rlang::is_installed("Matrix")
#' set.seed(1234)
#' mat <- matrix(sample(0:1, 100, TRUE, c(0.9, 0.1)), nrow = 10)
#' colnames(mat) <- letters[1:10]
#' sparse_mat <- Matrix::Matrix(mat, sparse = TRUE)
#' sparse_mat
#'
#' res <- coerce_to_sparse_data_frame(sparse_mat)
#' res
#'
#' # All columns are sparse
#' vapply(res, is_sparse_vector, logical(1))
#'
#' # Data frames can be used as well
#' res <- coerce_to_sparse_data_frame(as.data.frame(mat))
#' vapply(res, is_sparse_vector, logical(1))
#' @export
coerce_to_sparse_data_frame <- function(x, call = rlang::caller_env(0)) {
  if (inherits(x, "data.frame")) {
    return(.data_frame_to_sparse(x, call = call))
  }

  if (!any(methods::is(x) == "sparseMatrix")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls sparseMatrix} or {.cls data.frame}, \\
      not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  if (!methods::is(x, "dgCMatrix")) {
    x <- methods::as(x, "generalMatrix")
    x <- methods::as(x, "CsparseMatrix")
  }

  if (is.null(colnames(x))) {
    cli::cli_abort(
      "{.arg x} must have column names.",
      call = call
    )
  }

  res <- .sparse_matrix_to_list(x)
  names(res) <- make.names(names(res), unique = TRUE)
  attr(res, "row.names") <- .set_row_names(nrow(x))
  class(res) <- "data.frame"
  res
}

.data_frame_to_sparse <- function(x, call) {
  supported <- vapply(x, .has_sparse_type, logical(1))

  if (!all(supported)) {
    offenders <- x[!supported]
    types <- vapply(offenders, \(col) class(col)[[1]], character(1))
    offenders <- paste0(names(offenders), " (", types, ")")
    cli::cli_abort(
      c(
        x = "All columns of {.arg x} must be integer, double, logical, or \\
        character vectors.",
        i = "Unsupported columns: {.field {offenders}}."
      ),
      call = call
    )
  }

  res <- lapply(x, .as_sparse_column)
  attr(res, "row.names") <- attr(x, "row.names")
  class(res) <- "data.frame"
  res
}

.has_sparse_type <- function(x) {
  !is.object(x) &&
    is.null(dim(x)) &&
    typeof(x) %in% c("integer", "double", "logical", "character")
}

.as_sparse_column <- function(x) {
  switch(
    typeof(x),
    integer = as_sparse_integer(x),
    double = as_sparse_double(x),
    logical = as_sparse_logical(x),
    character = as_sparse_character(x)
  )
}

.sparse_matrix_to_list <- function(x) {
  if (methods::is(x, "ngCMatrix")) {
    values <- NULL
  } else {
    if (any(x@x == 0, na.rm = TRUE)) {
      x <- Matrix::drop0(x)
    }
    values <- x@x
  }

  # Matrix guarantees sorted, unique, in-range positions, so we skip the
  # validation in sparse_double().
  res <- .Call(ffi_sparse_matrix_to_list, x@i, x@p, values, nrow(x))

  names(res) <- colnames(x)
  res
}
