#ifndef SPARSEVCTRS_SPARSE_UTILS_H
#define SPARSEVCTRS_SPARSE_UTILS_H

r_obj* sparse_extract_val(r_obj* x);

r_obj* sparse_extract_pos(r_obj* x);

r_ssize sparse_extract_len(r_obj* x);

r_obj* sparse_extract_default(r_obj* x);

double sparse_extract_default_double(r_obj* x);

int sparse_extract_default_integer(r_obj* x);

r_obj* sparse_extract_default_string(r_obj* x);

int sparse_extract_default_logical(r_obj* x);

bool sparse_is_altrep(r_obj* x);

r_ssize sparse_binary_search(int needle, const int* v_haystack, r_ssize size);

bool sparse_is_index_handleable(r_obj* x);

void sparse_verbose_materialize(void);

void sparse_sort_pos_and_val(r_obj* pos, r_obj* val);

int sparse_ssize_as_int(r_ssize x);

#endif
