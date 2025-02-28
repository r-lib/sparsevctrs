#' Calculate median from sparse vectors
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
#' sparse_median(
#'   sparse_double(1000, 1, 1000)
#' )
#'
#' sparse_median(
#'   sparse_double(1000, 1, 1000, default = 1)
#' )
#'
#' sparse_median(
#'   sparse_double(c(10, 50, 11), c(1, 50, 111), 1000)
#' )
#'
#' sparse_median(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
#' )
#'
#' sparse_median(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
#'   na_rm = TRUE
#' )
#'
#' @export
sparse_median <- function(x, na_rm = FALSE) {
  default <- sparse_default(x)
  values <- sparse_values(x)
  values_len <- length(values)

  if (values_len == 0) {
    return(default)
  }

  x_len <- length(x)

  if ((x_len / 2) > values_len) {
    if (na_rm) {
      return(default)
    } else {
      if (any(is.na(values))) {
        return(NA_real_)
      } else {
        return(default)
      }
    }
  }

  stats::median(x, na.rm = na_rm)
}
