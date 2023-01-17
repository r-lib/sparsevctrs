#' Convert sparse matrices to tibbles
#'
#' @param x a sparse matrix
#'
#' @return a tibble
#' @export
#'
#' @seealso [tibble_to_sparse()]
#'
#' @examplesIf rlang::is_installed("rsparse")
#' data("movielens100k", package = "rsparse")
#'
#' sparse_to_tibble(movielens100k)
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

#' Convert tibbles to sparse matrices
#'
#' @param x a tibble
#'
#' @return a sparse matrix
#' @export
#'
#' @seealso [sparse_to_tibble()]
#'
#' @examples
#' mts <- mtcars
#' mts$new <- new_sparse_vector(c(1, 4, 8), c(8, 1, 20), nrow(mts))
#'
#' tibble_to_sparse(mtcars)
#'
#' tibble_to_sparse(mts)
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
