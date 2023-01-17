.positions <- function(x) {
  if (inherits(x, "sparse_vector")) {
    res <- attr(x, "positions")
  } else {
    res <- seq_along(x)
  }
  res
}

.values <- function(x) {
  if (inherits(x, "sparse_vector")) {
    res <- attr(x, "values")
  } else {
    res <- x
  }
  res
}
