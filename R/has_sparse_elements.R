#' Check for sparse elements
#' 
#' This function checks to see if a data.frame, tibble or list contains one or
#' more sparse vectors.
#' 
#' @param x a data frame, tibble, or list.
#'
#' @details
#' The checking in this function is done using [is_sparse_vector()], but is 
#' implemented using an early exit pattern to provide fast performance for wide
#' data.frames.
#' 
#' This function does not test whether `x` is a data.frame, tibble or list. It
#' simply iterates over the elements and sees if they are sparse vectors.
#' 
#' @return A single logical value.
#' 
#' @examplesIf rlang::is_installed("Matrix")
#' set.seed(1234)
#' n_cols <- 10000
#' mat <- matrix(sample(0:1, n_cols * 10, TRUE, c(0.9, 0.1)), ncol = n_cols)
#' colnames(mat) <- as.character(seq_len(n_cols))
#' sparse_mat <- Matrix::Matrix(mat, sparse = TRUE)
#' 
#' res <- coerce_to_sparse_tibble(sparse_mat)
#' has_sparse_elements(res)
#' 
#' has_sparse_elements(mtcars)
#' @export
has_sparse_elements <- function(x) {
  res <- FALSE

  for (elt in x) {
    if (is_sparse_vector(elt)) {
      res <- TRUE
      break
    }
  }
  res
}