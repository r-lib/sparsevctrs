#' Create sparse numeric vector
#' 
#' @param value Numeric vector, values of non-zero entries.
#' @param position integer vector, indices of non-zero entries.
#' @param length Integer, Length of vector.
#'
#' @export
new_sparse_real <- function(value, position, length) {
  check_number_whole(length, min = 0)
  if (!is.integer(length)) {
    length <- as.integer(length)
  }

  if (!is.numeric(value)) {
    cli::cli_abort(
      "{.arg value} must be a numeric vector, not {.obj_type_friendly {value}}."
    )
  }

  if (is.integer(value)) {
    value <- as.double(value)
  }

  if (!is.numeric(position)) {
    cli::cli_abort(
      "{.arg position} must be a integer vector, \\
      not {.obj_type_friendly {value}}."
    )
  }

  if (!is.integer(position)) {
    position <- vec_cast(position, integer())
  }

  len_value <- length(value)
  len_position <- length(position)

  if (len_value != len_position) {
    cli::cli_abort(
      "{.arg value} ({len_value}) and {.arg position} ({len_position}) \\
      must have the same length."
    )
  }

  x <- list(
    val = value, 
    pos = position, 
    len = length
  )

  .Call(ffi_altrep_new_sparse_real, x)
}
