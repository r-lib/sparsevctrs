#' Create sparse double vector
#'
#' Construction of vectors where only values and positions are recorded. The
#' Length and default values determine all other information.
#'
#' @param values double vector, values of non-zero entries.
#' @param positions integer vector, indices of non-zero entries.
#' @param length integer value, Length of vector.
#' @param default double value, value at indices not specified by `positions`.
#'   Defaults to `0`. Cannot be `NA`.
#'
#' @details
#'
#' `values` and `positions` are expected to be the same length, and are allowed
#' to both have zero length.
#'
#' Allowed values for `value` is double and integer values. integer values will
#' be coerced to doubles. Missing values such as `NA` and `NA_real_` are
#' allowed. Everything else is disallowed, This includes `Inf` and `NaN`. The
#' values are also not allowed to take the same value as `default`.
#'
#' `positions` should be integers or integer-like doubles. Everything else is
#' not allowed. Positions should furthermore be positive (`0` not allowed),
#' unique, and in increasing order. Lastly they should all be smaller that
#' `length`.
#'
#' For developers:
#'
#' setting `options("sparsevctrs.verbose_materialize" = TRUE)` will print a
#' message each time a sparse vector has been forced to materialize.
#'
#' @return sparse double vector
#'
#' @seealso [sparse_integer()] [sparse_character()]
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
sparse_double <- function(values, positions, length, default = 0) {
  check_number_decimal(default)
  validate_length(length)

  if (!is.integer(length)) {
    length <- as.integer(length)
  }

  if (is.integer(default)) {
    default <- as.numeric(default)
  }

  if (identical(values, NA)) {
    values <- NA_real_
  }

  validate_values_double(values)

  if (is.integer(values)) {
    values <- as.double(values)
  }

  validate_positions(positions, length, len_values = length(values))
  positions <- as.integer(positions)

  if (any(values == default, na.rm = TRUE)) {
    offenders <- which(values == default)
    cli::cli_abort(
      c(
        x = "{.arg values} value must not be equal to the default {default}.",
        i = "{default} values at index: {offenders}."
      )
    )
  }

  new_sparse_double(values, positions, length, default)
}

new_sparse_double <- function(values, positions, length, default) {
  x <- list(
    values,
    positions,
    length,
    default
  )

  .Call(ffi_altrep_new_sparse_double, x)
}
