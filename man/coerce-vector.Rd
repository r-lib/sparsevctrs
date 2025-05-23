% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coerce-vector.R
\name{coerce-vector}
\alias{coerce-vector}
\alias{as_sparse_double}
\alias{as_sparse_integer}
\alias{as_sparse_character}
\alias{as_sparse_logical}
\title{Coerce numeric vector to sparse double}
\usage{
as_sparse_double(x, default = 0)

as_sparse_integer(x, default = 0L)

as_sparse_character(x, default = "")

as_sparse_logical(x, default = FALSE)
}
\arguments{
\item{x}{a numeric vector.}

\item{default}{default value to use. Defaults to \code{0}.

The values of \code{x} must be double or integer. It must not contain any \code{Inf} or
\code{NaN} values.}
}
\value{
sparse vectors
}
\description{
Takes a numeric vector, integer or double, and turn it into a sparse double
vector.
}
\examples{
x_dense <- c(3, 0, 2, 0, 0, 0, 4, 0, 0, 0)
x_sparse <- as_sparse_double(x_dense)
x_sparse

is_sparse_double(x_sparse)
}
