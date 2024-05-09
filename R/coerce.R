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

  res <- .sparse_matrix_to_list(res)
  res <- tibble::as_tibble(res)
  res
}


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

  res <- .sparse_matrix_to_list(res)
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