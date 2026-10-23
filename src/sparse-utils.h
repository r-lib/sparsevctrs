#ifndef SPARSEVCTRS_SPARSE_UTILS_H
#define SPARSEVCTRS_SPARSE_UTILS_H

SEXP sparse_extract_val(SEXP x);

SEXP sparse_extract_pos(SEXP x);

R_xlen_t sparse_extract_len(SEXP x);

SEXP sparse_extract_default(SEXP x);

double sparse_extract_default_double(SEXP x);

int sparse_extract_default_integer(SEXP x);

SEXP sparse_extract_default_string(SEXP x);

Rboolean sparse_extract_default_logical(SEXP x);

bool sparse_is_altrep(SEXP x);

R_xlen_t sparse_binary_search(int needle, const int* v_haystack, R_xlen_t size);

bool sparse_is_index_handleable(SEXP x);

void sparse_verbose_materialize(void);

void sparse_sort_pos_and_val(SEXP pos, SEXP val);

#endif
