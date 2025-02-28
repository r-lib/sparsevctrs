#' Which indices are Missing Values
#'
#' @param x A sparse vector.
#'
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#'
#' @return A logical vector.
#'
#' @seealso [sparse_is_na()]
#'
#' @examples
#' sparse_which_na(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
#' )
#' @export
sparse_which_na <- function(x) {
  values <- sparse_values(x)

  res <- integer()

  na_values <- is.na(values)
  if (any(na_values)) {
    positions <- sparse_positions(x)
    res <- positions[na_values]
  }

  res
}
