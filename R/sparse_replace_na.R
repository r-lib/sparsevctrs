#' Replace NAs with specified values in sparse vectors
#'
#' @param x A sparse vector.
#' @param replace A single value.
#'
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#' The `replace` is likewise not type or length checked.
#'
#' The output type will match the values after coercion happens during
#' replacement.
#'
#' @return A sparse vector.
#'
#' @examples
#' sparse_replace_na(
#'   sparse_double(c(10, NA, 11), c(1, 5, 10), 10),
#'   5
#' )
#'
#' sparse_replace_na(
#'   sparse_integer(c(10L, NA, 11L), c(1, 5, 10), 10),
#'   5L
#' )
#'
#' sparse_replace_na(
#'   sparse_character(c("A", NA, "E"), c(2, 5, 10), 10),
#'   "missing"
#' )
#' @export
sparse_replace_na <- function(x, replace) {
  default <- sparse_default(x)
  values <- sparse_values(x)
  positions <- sparse_positions(x)
  len_values <- length(values)

  if (len_values == 0) {
    return(x)
  }

  if (replace == default) {
    remove <- is.na(values)
    values <- values[!remove]
    positions <- positions[!remove]
  } else {
    values[is.na(values)] <- replace
  }

  if (is.integer(values)) {
    res <- sparse_integer(
      values = values,
      positions = positions,
      length = length(x),
      default = default
    )
  } else if (is.double(values)) {
    res <- sparse_double(
      values = values,
      positions = positions,
      length = length(x),
      default = default
    )
  } else if (is.character(values)) {
    res <- sparse_character(
      values = values,
      positions = positions,
      length = length(x),
      default = default
    )
  }

  res
}
