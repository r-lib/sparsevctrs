#' Calculate sparsity of data frames, matrices, and sparse matrices
#'
#' Turning data frame with sparse columns into sparse matrix using
#' [Matrix::sparseMatrix()].
#'
#' @param x a data frame, matrix of sparse matrix.
#' @param sample a integer or `NULL`. Number of rows to sample to estimate
#'   sparsity. If `NULL` then no sampling is performed. Will not be used when
#'   `x` is a sparse matrix. Defaults to `NULL`.
#'
#' @details
#' Only numeric 0s are considered zeroes in this calculations. Missing values,
#' logical vectors and then string `"0"` aren't counted.
#'
#' @return a single number, between 0 and 1.
#'
#' @examples
#'
#' # data frame
#' sparsity(mtcars)
#'
#' # Matrix
#' set.seed(1234)
#' mat <- matrix(sample(0:1, 100, TRUE, c(0.9, 0.1)), nrow = 10)
#' colnames(mat) <- letters[1:10]
#'
#' sparsity(mat)
#'
#' # Sparse matrix
#' sparse_mat <- Matrix::Matrix(mat, sparse = TRUE)
#'
#' sparsity(sparse_mat)
#' @export
sparsity <- function(x, sample = NULL) {
  check_number_whole(sample, min = 1, allow_null = TRUE)

  x_type <- input_type(x)

  if (x_type != "sparse_matrix") {
    nrows <- nrow(x)
    if (!is.null(sample)) {
      if (nrows < sample) {
        sample <- nrows
      }
      x <- x[sample(nrows, sample), ]
    }
  }

  res <- switch(
    x_type,
    data.frame = sparsity_df(x),
    matrix = sparsity_mat(x),
    sparse_matrix = sparsity_sparse_mat(x)
  )

  res
}

input_type <- function(x, call = rlang::caller_env()) {
  if (is.data.frame(x)) {
    return("data.frame")
  } else if (is.matrix(x)) {
    return("matrix")
  } else if (any(methods::is(x) == "sparseMatrix")) {
    return("sparse_matrix")
  } else {
    cli::cli_abort(
      "{.arg x} must be a data frame, matrix, or sparse matrix,
       Not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}

count_zeroes <- function(x) {
  if (!inherits(x, c("numeric", "integer"))) {
    return(0)
  }

  if (is_sparse_vector(x)) {
    default <- sparse_default(x)
    values <- sparse_values(x)
    len <- length(x)

    if (default == 0) {
      res <- len - length(values)
    } else {
      res <- length(values)
    }
  } else {
    res <- sum(x == 0, na.rm = TRUE)
  }
  res
}

sparsity_df <- function(x) {
  res <- vapply(x, count_zeroes, double(1))
  res <- sum(res) / (nrow(x) * ncol(x))
  res
}

sparsity_mat <- function(x) {
  if (!is.numeric(x)) {
    return(0)
  }
  sum(x == 0, na.rm = TRUE) / length(x)
}

sparsity_sparse_mat <- function(x) {
  1 - (length(x@x) / length(x))
}
