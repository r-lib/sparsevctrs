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

  if (anyDuplicated(position) > 0) {
    offenders <- which(duplicated(position))

    cli::cli_abort(
      c(
        x = "{.arg position} must not contain any duplicate values.",
        i = "Duplicate values at index: {offenders}."
      )
    )
  }

  if (is.unsorted(position)) {
    cli::cli_abort(
      "{.arg position} must be sorted in increasing order."
    )
  }

  if (len_position > 0 && max(position) > length) {
    offenders <- which(position > length)
    cli::cli_abort(
      c(
        x = "{.arg position} value must not be larger than {.arg length}.",
        i = "Offending values at index: {offenders}."
      )
    )
  }

  if (len_position > 0 && min(position) < 1) {
    offenders <- which(position < 1)
    cli::cli_abort(
      c(
        x = "{.arg position} value must positive.",
        i = "Non-positive values at index: {offenders}."
      )
    )
  }

  if (any(value == 0)) {
    offenders <- which(value == 0)
    cli::cli_abort(
      c(
        x = "{.arg value} value must not be 0.",
        i = "0 values at index: {offenders}."
      )
    )
  }

  x <- list(
    val = value, 
    pos = position, 
    len = length
  )

  .Call(ffi_altrep_new_sparse_real, x)
}
