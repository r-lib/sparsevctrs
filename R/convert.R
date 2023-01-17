sparse_to_tibble <- function(x) {
  start <- x@p[seq(1, length(x@p) - 1)] + 1
  end <- x@p[seq(2, length(x@p))]

  column_length <- nrow(x)

  results <- list()

  for (column in seq_along(start)) {
    index <- seq(start[column], end[column])
    results[[column]] <- new_sparse_vector(
      values = x@x[index],
      positions = x@i[index],
      length = column_length
    )
  }

  names(results) <- colnames(x)

  tibble::as_tibble(results)
}

tibble_to_sparse <- function(x) {
  any_sparse_vector <- any(
    vapply(x, inherits, "sparse_vector", FUN.VALUE = logical(1))
  )

  if (any_sparse_vector) {
    all_positions <- lapply(x, .positions)
    all_values <- lapply(x, .values)
    all_rows <- rep(seq_along(x), times = lengths(all_positions))

    all_positions <- unlist(all_positions, use.names = FALSE)
    all_values <- unlist(all_values, use.names = FALSE)

    non_zero <- all_values != 0
    all_positions <- all_positions[non_zero]
    all_values <- all_values[non_zero]
    all_rows <- all_rows[non_zero]

    res <- Matrix::sparseMatrix(
      i = all_positions,
      j = all_rows,
      x = all_values
    )
  } else {
    res <- Matrix::Matrix(as.matrix(x), sparse = TRUE)
  }
  res
}
