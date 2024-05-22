#' Create sparse logical vector
#' 
#' Construction of vectors where only values and positions are recorded. The 
#' Length and default values determine all other information.
#' 
#' @param values logical vector, values of non-zero entries.
#' @param positions integer vector, indices of non-zero entries.
#' @param length integer value, Length of vector.
#' @param default logical value, value at indices not specified by `positions`. 
#'   Defaults to `FALSE`. Cannot be `NA`.
#' 
#' @details
#' 
#' `values` and `positions` are expected to be the same length, and are allowed
#' to both have zero length.
#' 
#' Allowed values for `value` are logical values. Missing values such as `NA` 
#' and `NA_real_` are allowed. Everything else is disallowed, The values are 
#' also not allowed to take the same value as `default`.
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
#' @return sparse logical vector
#' 
#' @seealso [sparse_double()] [sparse_integer()] [sparse_character()] 
#'  
#' @examples
#' sparse_logical(logical(), integer(), 10)
#' 
#' sparse_logical(c(TRUE, NA, TRUE), c(2, 5, 10), 10)
#' 
#' str(
#'   sparse_logical(c(TRUE, NA, TRUE), c(2, 5, 10), 1000000000)
#' )
#' @export
sparse_logical <- function(values, positions, length, default = FALSE) {
  check_bool(default)
  validate_length(length)
  
  if (!is.integer(length)) {
    length <- as.integer(length)
  }

  validate_values_logical(values)
  
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

  new_sparse_logical(values, positions, length, default)
}

new_sparse_logical <- function(values, positions, length, default) {
  x <- list(
    val = values,
    pos = positions,
    len = length,
    default = default
  )

  .Call(ffi_altrep_new_sparse_logical, x)
}
