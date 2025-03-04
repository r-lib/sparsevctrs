#' Detect Pressence of Missing Values
#'
#' @param x A sparse vector.
#' @param type A single string. Most be one of `logical` or `integer`.
#' Determines the resulting vector. If `type = integer` then a sparse vector is
#' returned.
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
#' @return A logical vector or sparse integer vector.
#'
#' @examples
#' sparse_is_na(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000)
#' )
#'
#' sparse_is_na(
#'   sparse_double(c(10, NA, 11), c(1, 50, 111), 1000),
#'   type = "integer"
#' )
#' @export
sparse_is_na <- function(x, type = "logical") {
  values <- sparse_values(x)

  na_values <- is.na(values)
  positions <- sparse_positions(x)
  positions <- positions[na_values]

  if (type == "logical") {
    res <- logical(length(x))

    if (any(na_values)) {
      res[positions] <- TRUE
    }
  } else {
    res <- sparse_integer(
      rep(1, length(positions)),
      positions,
      length(x),
      sparse_default(x)
    )
  }

  res
}
