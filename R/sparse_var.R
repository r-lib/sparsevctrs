#' Calculate variance from sparse vectors
#' 
#' @param x A sparse numeric vector.
#' @param na_rm Logical, whether to remove missing values. Defaults to `FALSE`.
#' 
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#' 
#' Much like [var()] it uses the denominator `n-1`.
#'
#' @return single numeric value.
#' 
#' @examples
#' sparse_var(
#'   sparse_double(1000, 1, 1000)
#' )
#' 
#' sparse_var(
#'   sparse_double(1000, 1, 1000, default = 1)
#' )
#' 
#' sparse_var(
#'   sparse_double(c(10, 50, 11), c(1, 50, 111), 1000)
#' )
#' 
#' sparse_var(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
#' )
#' 
#' sparse_var(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
#'   na_rm = TRUE
#' )
#' 
#' @export
sparse_var <- function(x, na_rm = FALSE) {
  values <- sparse_values(x)
  len_values <- length(values)
  
  if (len_values == 0) {
    return(0)
  }

  default <- sparse_default(x)
  x_len <- length(x)

  mean <- sparse_mean(x, na_rm = na_rm)

  res <- sum((values - mean) ^ 2, na.rm = na_rm)


  res <- res + (default - mean) ^ 2 * (x_len - len_values)
  
  denominator <- x_len - 1

  if (na_rm) {
    denominator <- denominator - sum(is.na(values))
  }

  res <- res / denominator
  res
}
