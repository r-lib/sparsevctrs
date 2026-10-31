#' Calculate variance from sparse vectors
#'
#' @param x A sparse numeric vector.
#' @param na_rm Logical, whether to remove missing values. Defaults to `FALSE`.
#'
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a numeric vector. This is done for performance reasons, and it
#' is thus the users responsibility to perform input checking. Non-sparse
#' numeric vectors are supported but get no performance benefit.
#'
#' Like [var()], it returns `NA` when fewer than 2 non-missing observations
#' remain.
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
  x_len <- length(x)

  if (x_len < 2) {
    return(NA_real_)
  }

  values <- sparse_values(x)
  len_values <- length(values)
  default <- sparse_default(x)

  denominator <- x_len - 1

  if (na_rm) {
    denominator <- denominator - sum(is.na(values))
  }

  if (denominator < 1) {
    return(NA_real_)
  }

  if (len_values == 0) {
    return(0)
  }

  mean <- sparse_mean(x, na_rm = na_rm)

  res <- sum((values - mean)^2, na.rm = na_rm)

  if (!is.na(default)) {
    res <- res + (default - mean)^2 * as.numeric(x_len - len_values)
  }

  res / denominator
}
