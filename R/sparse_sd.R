#' Calculate standard diviation from sparse vectors
#' 
#' @param x A sparse numeric vector.
#' @param na_rm Logical, whether to remove missing values. Defaults to `FALSE`.
#' 
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#' 
#' Much like [sd()] it uses the denominator `n-1`.
#'
#' @return single numeric value.
#' 
#' @examples
#' sparse_sd(
#'   sparse_double(1000, 1, 1000)
#' )
#' 
#' sparse_sd(
#'   sparse_double(1000, 1, 1000, default = 1)
#' )
#' 
#' sparse_sd(
#'   sparse_double(c(10, 50, 11), c(1, 50, 111), 1000)
#' )
#' 
#' sparse_sd(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
#' )
#' 
#' sparse_sd(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
#'   na_rm = TRUE
#' )
#' 
#' @export
sparse_sd <- function(x, na_rm = FALSE) {
  sqrt(sparse_var(x, na_rm = na_rm))
}
