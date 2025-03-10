#ifndef SPARSEVCTRS_SPARSE_UTILS_H
#define SPARSEVCTRS_SPARSE_UTILS_H

#define R_NO_REMAP
#include <Rinternals.h>
#include <stdbool.h>

#define SVCTRS_DATAPTR(x) (void*) DATAPTR_RO(x);

SEXP extract_val(SEXP x);

SEXP extract_pos(SEXP x);

R_xlen_t extract_len(SEXP x);

SEXP extract_default(SEXP x);

double extract_default_double(SEXP x);

int extract_default_integer(SEXP x);

SEXP extract_default_string(SEXP x);

Rboolean extract_default_logical(SEXP x);

bool is_altrep(SEXP x);

SEXP ffi_extract_altrep_class(SEXP x);

SEXP ffi_is_sparse_vector(SEXP x);

SEXP ffi_is_altrep_non_sparse_vector(SEXP x);

R_xlen_t binary_search(int needle, const int* v_haystack, R_xlen_t size);

bool is_index_handleable(SEXP x);

void verbose_materialize(void);

void sort_pos_and_val(SEXP pos, SEXP val);

#endif
