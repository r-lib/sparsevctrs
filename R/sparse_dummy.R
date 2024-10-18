#' Generate sparse dummy variables
#' 
#' @param x A factor.
#' 
#' @details
#' Only factor variables can be used with [sparse_dummy()]. A call to 
#' `as.factor()` would be required for any other type of data.
#'
#' @return A list of sparse integer dummy variables.
#' 
#' @examples
#' x <- factor(c("a", "a", "b", "c", "d", "b"))
#' 
#' sparse_dummy(x)
#' @export
sparse_dummy <- function(x) {
  if (!is.factor(x)) {
    cli::cli_abort("{.arg x} must be a factor, not {.obj_type_friendly {x}}.")
  }

  lvls <- levels(x)

  x <- as.integer(x)
  
  counts <- tabulate(x, nbins = length(lvls))

  res <- .Call(ffi_sparse_dummy, x, lvls, counts)
  names(res) <- lvls
  res
}
