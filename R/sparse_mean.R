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

  default <- sparse_default(x)
  values <- sparse_values(x)
  len_values <- length(values)

  if (len_values == 0) {
    return(default)
  }

  x_len <- length(x)

  res <- sum(values, na.rm = na_rm)

  if (!is.na(default) && default != 0) {
    res <- res + (x_len - len_values) * default
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
