#' Create new sparse vector
#'
#' @param values Vector of values
#' @param positions Vector of positions, must be integers and same length as
#'   values.
#' @param length Length of resulting vector
#'
#' @return A sparse_vector object.
#' @export
#'
#' @examples
#' new_sparse_vector(1, 4, 10)
new_sparse_vector <- function(values, positions, length) {
  vec_assert(values, double())
  vec_assert(values, double())
  vec_assert(values, double())
  res <- seq(length)
  res <- new_vctr(res, class = "sparse_vector")
  attr(res, "values") <- values
  attr(res, "positions") <- positions
  res
}

#' @export
format.sparse_vector <- function(x, ...) {
  out <- rep(0, length(x))
  out[attr(x, "positions")] <- attr(x, "values")
  out
}

#' @export
vec_ptype_abbr.sparse_vector <- function(x, ...) {
  "spvtr"
}

#' @export
vec_math.sparse_vector <- function(.fn, .x, ...) {
  switch(
    .fn,
    sum = sum(attr(.x, "values")),
    prod = ifelse(length(attr(.x, "values")) != length(.x), 0, prod(attr(.x, "values"))),
    mean = sum(attr(.x, "values")) / length(.x),
    vec_math_base(.fn, .x, ...)
  )
}

sparse_vector_addition <- function(x, y) {
  res <- y

  values <- attr(x, "values")
  positions <- attr(x, "positions")

  overlap <- positions %in% attr(res, "positions")

  res_loc <- match(positions[overlap], attr(res, "positions"))

  attr(res, "values")[res_loc] <- attr(res, "values")[res_loc] + values[overlap]

  attr(res, "values") <- c(attr(res, "values"), values[!overlap])

  attr(res, "positions") <- c(attr(res, "positions"), positions[!overlap])
  res
}

#' @export
#' @method vec_arith sparse_vector
vec_arith.sparse_vector <- function(op, x, y, ...) {
  switch(
    op,
    "+" = sparse_vector_addition(x, y),
    stop_incompatible_op(op, x, y)
  )
}
