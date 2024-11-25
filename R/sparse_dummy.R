#' Generate sparse dummy variables
#' 
#' @param x A factor.
#' @param one_hot A single logical value. Should the first factor level be
#' included or not. Defaults to `FALSE`.
#' 
#' @details
#' Only factor variables can be used with [sparse_dummy()]. A call to 
#' `as.factor()` would be required for any other type of data.
#' 
#' If only a single level is present after `one_hot` takes effect. Then the 
#' vector produced won't be sparse.
#' 
#' A missing value at the `i`th element will produce missing values for all 
#' dummy variables at thr `i`th position.
#'
#' @return A list of sparse integer dummy variables.
#' 
#' @examples
#' x <- factor(c("a", "a", "b", "c", "d", "b"))
#' 
#' sparse_dummy(x, one_hot = FALSE)
#' 
#' x <- factor(c("a", "a", "b", "c", "d", "b"))
#' 
#' sparse_dummy(x, one_hot = TRUE)
#' 
#' x <- factor(c("a", NA, "b", "c", "d", NA))
#' 
#' sparse_dummy(x, one_hot = FALSE)
#' 
#' x <- factor(c("a", NA, "b", "c", "d", NA))
#' 
#' sparse_dummy(x, one_hot = TRUE)
#' @export
sparse_dummy <- function(x, one_hot = TRUE) {
  if (!is.factor(x)) {
    cli::cli_abort("{.arg x} must be a factor, not {.obj_type_friendly {x}}.")
  }

  lvls <- levels(x)

  x <- as.integer(x)
  
  if (!one_hot) {
    lvls <- lvls[-1]
    x <- x - 1L
  }

  n_lvls <- length(lvls)

  if (n_lvls == 1 && one_hot) {
    res <- list(rep(1L, length(x)))
    names(res) <- lvls
    return(res)
  }

  counts <- tabulate(x, nbins = n_lvls)

  if (anyNA(x)) {
    n_missing <- sum(is.na(x))
    counts <- counts + n_missing
    res <- .Call(ffi_sparse_dummy_na, x, lvls, counts, one_hot)
  } else {
    res <- .Call(ffi_sparse_dummy, x, lvls, counts, one_hot)
  }

  names(res) <- lvls
  res
}
