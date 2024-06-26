% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coerce.R
\name{coerce_to_sparse_matrix}
\alias{coerce_to_sparse_matrix}
\title{Coerce sparse data frame to sparse matrix}
\usage{
coerce_to_sparse_matrix(x)
}
\arguments{
\item{x}{a data frame or tibble with sparse columns.}
}
\value{
sparse matrix
}
\description{
Turning data frame with sparse columns into sparse matrix using
\code{\link[Matrix:sparseMatrix]{Matrix::sparseMatrix()}}.
}
\details{
No checking is currently do to \code{x} to determine whether it contains sparse
columns or not. Thus it works with any data frame. Needless to say, creating
a sparse matrix out of a dense data frame is not ideal.
}
\examples{
\dontshow{if (rlang::is_installed("Matrix")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
sparse_tbl <- lapply(1:10, function(x) sparse_double(x, x, length = 10))
names(sparse_tbl) <- letters[1:10]
sparse_tbl <- as.data.frame(sparse_tbl)
sparse_tbl

res <- coerce_to_sparse_matrix(sparse_tbl)
res
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=coerce_to_sparse_data_frame]{coerce_to_sparse_data_frame()}} \code{\link[=coerce_to_sparse_tibble]{coerce_to_sparse_tibble()}}
}
