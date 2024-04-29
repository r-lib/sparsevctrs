#ifndef SPARSEVCTRS_SPARSE_UTILS_H
#define SPARSEVCTRS_SPARSE_UTILS_H

#include <Rinternals.h>

SEXP extract_val(SEXP vec);

SEXP extract_pos(SEXP vec);

R_xlen_t extract_len(SEXP vec);

#endif
