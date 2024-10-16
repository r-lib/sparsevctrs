sparse_dummy <- function(x, lvls = NULL) {
  if (is.null(lvls)) {
    lvls <- levels(x)
  }
  
  x <- as.integer(x)
  
  counts <- tabulate(x)
  

  res <- .Call(ffi_sparse_dummy, x, lvls, counts)
  names(res) <- lvls
  res
}
