#' Detect Pressence of Missing Values
#'
#' @param x A sparse vector.
#'
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#'
#' Note that the resulting vector will be not be a sparse vector.
#'
#' @seealso [sparse_which_na()]
#'
#' @return A logical vector.
#'
#' @examples
#' sparse_is_na(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
#' )
#' @export
sparse_is_na <- function(x) {
  values <- sparse_values(x)

  res <- logical(length(x))

  na_values <- is.na(values)
  if (any(na_values)) {
    positions <- sparse_positions(x)
    res[positions[na_values]] <- TRUE
  }

  res
}
