#' Create sparse double vector
#' 
#' @param values Numeric vector, values of non-zero entries.
#' @param positions integer vector, indices of non-zero entries.
#' @param length Integer, Length of vector.
#' 
#' @details
#' 
#' For developers: 
#' 
#' setting `options("sparsevctrs.verbose_materialize" = TRUE)` will print a 
#' message each time a sparse vector has been forced to materialize.
#'
#' @examples
#' sparse_double(numeric(), integer(), 10)
#' 
#' sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 10)
#' 
#' str(
#'   sparse_double(c(pi, 5, 0.1), c(2, 5, 10), 1000000000)
#' )
#' @export
sparse_double <- function(values, positions, length) {
  check_number_whole(length, min = 0)
  if (!is.integer(length)) {
    length <- as.integer(length)
  }

  if (!is.numeric(values)) {
    cli::cli_abort(
      "{.arg values} must be a numeric vector, \\
      not {.obj_type_friendly {values}}."
    )
  }

  if (any(is.infinite(values))) {
    offenders <- which(is.infinite(values))
    cli::cli_abort(
      c(
        x = "{.arg values} must not contain infinite values.",
        i = "Infinite values at index: {offenders}."
      )
    )
  }

  if (any(is.nan(values))) {
    offenders <- which(is.nan(values))
    cli::cli_abort(
      c(
        x = "{.arg values} must not contain NaN values.",
        i = "NaN values at index: {offenders}."
      )
    )
  }

  if (is.integer(values)) {
    values <- as.double(values)
  }

  if (!is.numeric(positions)) {
    cli::cli_abort(
      "{.arg positions} must be a integer vector, \\
      not {.obj_type_friendly {positions}}."
    )
  }

  if (any(is.infinite(positions))) {
    offenders <- which(is.infinite(positions))
    cli::cli_abort(
      c(
        x = "{.arg positions} must not contain infinite values.",
        i = "Infinite values at index: {offenders}."
      )
    )
  }

  if (any(is.nan(positions))) {
    offenders <- which(is.nan(positions))
    cli::cli_abort(
      c(
        x = "{.arg positions} must not contain NaN values.",
        i = "NaN values at index: {offenders}."
      )
    )
  }

  if (!is.integer(positions)) {
    if (any(round(positions) != positions, na.rm = TRUE)) {
      offenders <- which(round(positions) != positions)

      cli::cli_abort(
        c(
          x = "{.arg positions} must contain integer values.",
          i = "Non-integer values at index: {offenders}."
        )
      )
    }
   
    positions <- as.integer(positions)
  }

  len_values <- length(values)
  len_positions <- length(positions)

  if (len_values != len_positions) {
    cli::cli_abort(
      "{.arg value} ({len_values}) and {.arg positions} ({len_positions}) \\
      must have the same length."
    )
  }

  if (anyDuplicated(positions) > 0) {
    offenders <- which(duplicated(positions))

    cli::cli_abort(
      c(
        x = "{.arg positions} must not contain any duplicate values.",
        i = "Duplicate values at index: {offenders}."
      )
    )
  }

  if (is.unsorted(positions)) {
    cli::cli_abort(
      "{.arg positions} must be sorted in increasing order."
    )
  }

  if (len_positions > 0 && max(positions) > length) {
    offenders <- which(positions > length)
    cli::cli_abort(
      c(
        x = "{.arg positions} value must not be larger than {.arg length}.",
        i = "Offending values at index: {offenders}."
      )
    )
  }

  if (len_positions > 0 && min(positions) < 1) {
    offenders <- which(positions < 1)
    cli::cli_abort(
      c(
        x = "{.arg positions} value must positive.",
        i = "Non-positive values at index: {offenders}."
      )
    )
  }

  if (any(values == 0)) {
    offenders <- which(values == 0)
    cli::cli_abort(
      c(
        x = "{.arg values} value must not be 0.",
        i = "0 values at index: {offenders}."
      )
    )
  }

  new_sparse_double(values, positions, length)
}

new_sparse_double <- function(values, positions, length) {
  x <- list(
    val = values, 
    pos = positions, 
    len = length
  )

  .Call(ffi_altrep_new_sparse_double, x)
}
