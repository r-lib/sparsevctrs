#ifndef SPARSEVCTRS_SPARSE_UTILS_H
#define SPARSEVCTRS_SPARSE_UTILS_H

#include <Rinternals.h>

SEXP extract_val(SEXP x);

SEXP extract_pos(SEXP x);

R_xlen_t extract_len(SEXP x);

SEXP is_altrep(SEXP x);

#endif
