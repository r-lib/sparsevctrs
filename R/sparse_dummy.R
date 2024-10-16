sparse_dummy <- function(x, lvls = NULL) {
  if (is.null(lvls)) {
    lvls <- levels(x)
  }
  
  x <- as.integer(x)
  
  counts <- tabulate(x)
  

  res <- .Call(ffi_sparse_dummy, x, lvls, counts)
  names(res) <- lvls
  
  lapply(res, create_dummy, length(x))
}

create_dummy <- function(x, len) {
  new_sparse_integer(rep(1L, length(x)), x, len, 0L)
}
