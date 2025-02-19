#' Calculate sqrt of sparse vectors
#'
#' @param x A sparse numeric vector.
#'
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#'
#' The output will be a double vector regardless of the input type.
#'
#' @return A sparse double vector.
#'
#' @examples
#' sparse_sqrt(
#'   sparse_double(1000, 1, 10)
#' )
#'
#' sparse_sqrt(
#'   sparse_integer(1000, 3, 10, default = 2)
#' )
#'
#' sparse_sqrt(
#'   sparse_double(c(10, NA, 11), c(1, 5, 10), 10)
#' )
#' @export
sparse_sqrt <- function(x) {
  default <- sparse_default(x)
  values <- sparse_values(x)
  positions <- sparse_positions(x)
  len_values <- length(values)

  if (len_values == 0 && default == 0) {
    return(x)
  }

  res <- sparse_double(
    values = sqrt(values),
    positions = positions,
    length = length(x),
    default = sqrt(default)
  )

  res
}
