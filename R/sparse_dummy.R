#' Generate sparse dummy variables
#' 
#' @param x A factor.
#' @param one_hot A single logical value. Should the first factor level be
#' ignored. Defaults to `FALSE`.
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
sparse_dummy <- function(x, one_hot = FALSE) {
  if (!is.factor(x)) {
    cli::cli_abort("{.arg x} must be a factor, not {.obj_type_friendly {x}}.")
  }

  lvls <- levels(x)

  x <- as.integer(x)
  
  if (one_hot) {
    lvls <- lvls[-1]
    x <- x - 1L
  }

  counts <- tabulate(x, nbins = length(lvls))

  res <- .Call(ffi_sparse_dummy, x, lvls, counts, one_hot)
  names(res) <- lvls
  res
}
