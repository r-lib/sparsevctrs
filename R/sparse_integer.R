#' Create sparse integer vector
#' 
#' Construction of vectors where only values and positions are recorded. The 
#' Length and default values determine all other information.
#' 
#' @param values integer vector, values of non-zero entries.
#' @param positions integer vector, indices of non-zero entries.
#' @param length integer value, Length of vector.
#' @param default integer value, value at indices not specified by `positions`. 
#'   Defaults to `0L`. Cannot be `NA`.
#' 
#' @details
#' 
#' `values` and `positions` are expected to be the same length, and are allowed
#' to both have zero length.
#' 
#' Allowed values for `value` is integer values. This means that the double 
#' vector `c(1, 5, 4)` is accepted as it can be losslessly converted to the
#' integer vector `c(1L, 5L, 4L)`. Missing values such as `NA` and `NA_real_` 
#' are allowed. Everything else is disallowed, This includes `Inf` and `NaN`. 
#' The values are also not allowed to take the same value as `default`.
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
#' @return sparse integer vector
#' 
#' @seealso [sparse_double()] [sparse_character()]
#' 
#' @examples
#' sparse_integer(integer(), integer(), 10)
#' 
#' sparse_integer(c(4, 5, 7), c(2, 5, 10), 10)
#' 
#' str(
#'   sparse_integer(c(4, 5, 7), c(2, 5, 10), 1000000000)
#' )
#' @export
sparse_integer <- function(values, positions, length, default = 0L) {
  check_number_whole(default)
  validate_length(length)
  
  if (!is.integer(length)) {
    length <- as.integer(length)
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

  values <- vctrs::vec_cast(values, integer())
  default <- vctrs::vec_cast(default, integer())
  
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

  new_sparse_integer(values, positions, length, default)
}

new_sparse_integer <- function(values, positions, length, default) {
  x <- list(
    values,
    positions,
    length,
    default
  )

  .Call(ffi_altrep_new_sparse_integer, x)
}
