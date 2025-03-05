#' Compute lagged values for sparse vectors
#'
#' @param x A sparse vector.
#' @param n Positive integer of length 1, giving the number of positions to lag
#'     by.
#' @param default The value used to pad `x`` back to its original size after
#'   the lag has been applied. The default, `NULL``, pads with a missing value.
#'
#' @details
#' This function, as with any of the other helper functions assumes that the
#' input `x` is a sparse numeric vector. This is done for performance reasons,
#' and it is thus the users responsibility to perform input checking.
#'
#' @return sparse vector.
#'
#' @examples
#' vec_dbl <- sparse_double(c(pi, 4, 5/2), c(1, 5, 7), 10)
#'
#' sparse_lag(vec_dbl)
#' sparse_lag(vec_dbl, n = 3)
#' sparse_lag(vec_dbl, n = 3, default = 0)
#'
#' vec_int <- sparse_integer(c(1, 2, 3), c(1, 5, 7), 10)
#'
#' sparse_lag(vec_int)
#' sparse_lag(vec_int, n = 3)
#' sparse_lag(vec_int, n = 3, default = 0L)
#'
#' vec_chr <- sparse_character(c("A", "B", "C"), c(1, 5, 7), 10)
#'
#' sparse_lag(vec_chr)
#' sparse_lag(vec_chr, n = 3)
#' sparse_lag(vec_chr, n = 3, default = "before")
#'
#' vec_lgl <- sparse_logical(c(TRUE, TRUE, TRUE), c(1, 5, 7), 10)
#'
#' sparse_lag(vec_lgl)
#' sparse_lag(vec_lgl, n = 3)
#' sparse_lag(vec_lgl, n = 3, default = FALSE)
#' @export
sparse_lag <- function(x, n = 1L, default = NULL) {
  values <- sparse_values(x)
  positions <- sparse_positions(x)
  len <- length(x)
  x_default <- sparse_default(x)

  n <- pmin(n, len)

  if (n < 1) {
    cli::cli_abort("{.arg n} must be at least 1, not {n}.")
  }

  positions <- positions + n

  overflow <- positions > len
  if (any(overflow)) {
    positions <- positions[!overflow]
    values <- values[!overflow]
  }

  if (!identical(x_default, default)) {
    if (is.null(default)) {
      default <- NA
    }

    values <- c(rep(default, n), values)
    positions <- c(seq_len(n), positions)
  }

  generator <- switch(
    class(x),
    integer = sparse_integer,
    numeric = sparse_double,
    character = sparse_character,
    logical = sparse_logical
  )

  generator(
    values = values,
    positions = positions,
    length = len,
    default = x_default
  )
}
