% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{sparsevctrs_options}
\alias{sparsevctrs_options}
\title{sparsevctrs options}
\description{
These options can be set with \code{options()}.
}
\details{
\subsection{sparsevctrs.verbose_materialize}{

This option is meant to be used as a diagnostic tool. Materialization of
sparse vectors are done silently by default. This can make it hard to
determine if your code is doing what you want.

Setting \code{sparsevctrs.verbose_materialize} is a way to alert when
materialization occurs. Note that only the first materialization is counted
for the options below, as the materialized vector is cached.

Setting \code{sparsevctrs.verbose_materialize = 1} or
\code{sparsevctrs.verbose_materialize = TRUE} will result in a message being
emitted each time a sparse vector is materialized.

Setting \code{sparsevctrs.verbose_materialize = 2} will result in a warning being
thrown each time a sparse vector is materialized.

Setting \code{sparsevctrs.verbose_materialize = 3} will result in an error being
thrown each time a sparse vector is materialized.
}
}
