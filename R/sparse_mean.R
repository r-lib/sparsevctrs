#' Calculate mean from sparse vectors
#'
#' @param x A sparse numeric vector.
#' @param wts A numeric vector, should be same length as `x`.
#' @param na_rm Logical, whether to remove missing values. Defaults to `FALSE`.
#'
#'
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#'
#' When `wts` is supplied, the weights are multiplied into `x` and the result is
#' divided by the sum of the weights. With `na_rm = TRUE`, weights at positions
#' where the weighted value is missing are dropped from the sum of the weights,
#' so a missing weight removes that observation instead of returning `NA` as
#' [stats::weighted.mean()] does.
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
sparse_mean <- function(x, wts = NULL, na_rm = FALSE) {
  if (!is.null(wts)) {
    x <- sparse_multiplication(x, wts)
  }

  if (length(x) == 0) {
    return(NaN)
  }

  default <- sparse_default(x)
  values <- sparse_values(x)
  len_values <- length(values)

  if (len_values == 0) {
    if (is.null(wts)) {
      return(default)
    }
    return(default / sum(wts))
  }

  x_len <- length(x)

  res <- sum(values, na.rm = na_rm)

  if (!is.na(default) && default != 0) {
    res <- res + as.numeric(x_len - len_values) * default
  }

  if (na_rm) {
    x_len <- x_len - sum(is.na(values))
  }

  if (is.null(wts)) {
    res <- res / x_len
  } else {
    na_loc <- sparse_which_na(x)
    if (length(na_loc) > 0) {
      wts <- wts[-na_loc]
    }
    res <- res / sum(wts)
  }

  res
}
