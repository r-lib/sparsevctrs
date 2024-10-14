#' Calculate mean from sparse vectors
#' 
#' Helper functions to determine whether an vector is a sparse vector or not.
#' 
#' @param x A sparse numeric vector.
#' @param na_rm Logical, whether to remove missing values. Defaults to `FALSE`.
#' 
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#'
#' @return single numeric value.
#' 
#' @examples
#' sparse_mean(
#'   sparse_double(1000, 1, 1000)
#' )
#' 
#' sparse_mean(
#'   sparse_double(1000, 1, 1000, default = 1)
#' )
#' 
#' sparse_mean(
#'   sparse_double(c(10, 50, 11), c(1, 50, 111), 1000)
#' )
#' 
#' sparse_mean(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
#' )
#' 
#' sparse_mean(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
#'   na_rm = TRUE
#' )
#' 
#' @export
sparse_mean <- function(x, na_rm = FALSE) {
  values <- sparse_values(x)
  x_len <- length(x)
  default <- sparse_default(x)

  res <- sum(values, na.rm = na_rm)

  if (default != 0) {
    res <- res + (x_len - length(values)) * default
  }
  
  res <- res / x_len
  
  res
}
