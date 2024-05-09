#' Coerce sparse data frame to sparse matrix
#' 
#' @param x a data frame or tibble with sparse columns.
#'
#' @seealso [coerce_to_sparse_data_frame()] [coerce_to_sparse_tibble()]
#' @examplesIf rlang::is_installed("Matrix")
#' set.seed(1234)
#' sparse_tbl <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
#' names(sparse_tbl) <- letters[1:10]
#' sparse_tbl <- as.data.frame(sparse_tbl)
#' sparse_tbl
#' 
#' res <- coerce_to_sparse_matrix(sparse_tbl)
#' res
#' @export
coerce_to_sparse_matrix <- function(x) {
  rlang::check_installed("Matrix")

  if (!inherits(x, "data.frame")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls data.frame}, not {.obj_type_friendly {x}}."
    )
  }

  all_positions <- lapply(x, sparse_positions)
  all_values <- lapply(x, sparse_values)
  all_rows <- rep(seq_along(x), times = lengths(all_positions))

  all_positions <- unlist(all_positions, use.names = FALSE)
  all_values <- unlist(all_values, use.names = FALSE)

  res <- Matrix::sparseMatrix(
    i = all_positions,
    j = all_rows,
    x = all_values,
    dimnames = list(rownames(x), colnames(x))
  )
  res
}

#' Coerce sparse matrix to tibble with sparse columns
#' 
#' @param x sparse matrix. 
#'
#' @seealso [coerce_to_sparse_data_frame()] [coerce_to_sparse_matrix()]
#' @examplesIf rlang::is_installed("Matrix")
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
coerce_to_sparse_tibble <- function(x) {
  rlang::check_installed("tibble")

  if (!any(methods::is(x) == "sparseMatrix")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls sparseMatrix}, not {.obj_type_friendly {x}}."
    )
  }

  if (is.null(colnames(x))) {
    cli::cli_abort(
      "{.arg x} must have column names."
    )
  }

  res <- .sparse_matrix_to_list(x)
  res <- tibble::as_tibble(res)
  res
}

#' Coerce sparse matrix to data frame with sparse columns
#' 
#' @param x sparse matrix. 
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
#' @export
coerce_to_sparse_data_frame <- function(x) {
  if (!any(methods::is(x) == "sparseMatrix")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls sparseMatrix}, not {.obj_type_friendly {x}}."
    )
  }

  if (is.null(colnames(x))) {
    cli::cli_abort(
      "{.arg x} must have column names."
    )
  }

  res <- .sparse_matrix_to_list(x)
  res <- as.data.frame(res)
  res
}

.sparse_matrix_to_list <- function(x) {
  values <- x@x
  positions <- x@i
  
  x_length <- nrow(x)
  
  res <- list()
  for (i in seq_len(ncol(x))) {
    start <- x@p[i] + 1
    end <- x@p[i + 1]
  
    index <- seq(start, end)
  
    res[[i]] <- sparse_double(
      values = values[index],
      positions = positions[index] + 1,
      length = x_length
    )
  }
  
  names(res) <- colnames(x)
  res
}