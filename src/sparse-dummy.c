#include "sparsevctrs-core.h"

#include "sparse-dummy.h"
#include "sparse-utils.h"
#include "altrep-sparse-integer.h"

#include "decl/sparse-dummy-decl.h"

r_obj* ffi_sparse_dummy(r_obj* ffi_x, r_obj* ffi_lvls, r_obj* ffi_one_hot) {
  const r_ssize n_lvls = r_length(ffi_lvls);
  const r_ssize len = sparse_ssize_as_int(r_length(ffi_x));

  const int* v_x = r_int_cbegin(ffi_x);
  const unsigned int shift = r_lgl_get(ffi_one_hot, 0) == 1 ? 1 : 2;

  r_obj* counts = KEEP(r_alloc_integer(n_lvls + 2));
  int* v_counts = r_int_begin(counts);

  sparse_dummy_count(v_x, len, shift, n_lvls, v_counts);

  const r_ssize n_na = v_counts[n_lvls];

  r_obj* out_len = KEEP(r_int((int) len));
  r_obj* out_default = KEEP(r_int(0));

  r_obj* out;
  if (n_na == 0) {
    out = sparse_dummy_no_na(
      v_x,
      len,
      shift,
      n_lvls,
      v_counts,
      out_len,
      out_default
    );
  } else {
    out = sparse_dummy_na(
      v_x,
      len,
      shift,
      n_lvls,
      v_counts,
      n_na,
      out_len,
      out_default
    );
  }

  FREE(3);

  return out;
}

static void sparse_dummy_count(
  const int* v_x,
  r_ssize len,
  unsigned int shift,
  r_ssize n_lvls,
  int* v_counts
) {
  for (r_ssize k = 0; k < n_lvls + 2; ++k) {
    v_counts[k] = 0;
  }

  const unsigned int n_lvls_uint = (unsigned int) n_lvls;

  for (r_ssize i = 0; i < len; ++i) {
    const int code = v_x[i];
    const unsigned int k = (unsigned int) code - shift;
    const unsigned int other = n_lvls_uint + (code != r_globals.na_int);

    ++v_counts[k < n_lvls_uint ? k : other];
  }
}

static r_obj* sparse_dummy_no_na(
  const int* v_x,
  r_ssize len,
  unsigned int shift,
  r_ssize n_lvls,
  const int* v_counts,
  r_obj* out_len,
  r_obj* out_default
) {
  // Generate list of integer vectors. One vector for each level, with its
  // length equal to the number of occurances of that level.
  r_obj* out = KEEP(r_alloc_list(n_lvls));
  r_obj* ptrs = KEEP(r_alloc_raw((n_lvls + 1) * sizeof(int*)));
  int** v_ptrs = (int**) r_raw_begin(ptrs);

  for (r_ssize k = 0; k < n_lvls; ++k) {
    r_obj* pos = r_alloc_integer(v_counts[k]);
    r_list_poke(out, k, pos);
    v_ptrs[k] = r_int_begin(pos);
  }

  int scratch;
  v_ptrs[n_lvls] = &scratch;

  const unsigned int n_lvls_uint = (unsigned int) n_lvls;

  for (r_ssize i = 0; i < len; ++i) {
    const unsigned int k = (unsigned int) v_x[i] - shift;
    const unsigned int slot = k < n_lvls_uint ? k : n_lvls_uint;

    int* v_pos = v_ptrs[slot];

    // we need the result to be 1-indexed
    *v_pos = i + 1;
    v_ptrs[slot] = v_pos + (slot < n_lvls_uint);
  }

  // Turn list of integer vectors with positions, into list of sparse integer
  // vectors.
  for (r_ssize k = 0; k < n_lvls; ++k) {
    r_obj* pos = r_list_get(out, k);

    const r_ssize n_val = v_counts[k];
    r_obj* val = KEEP(r_alloc_integer(n_val));
    int* v_val = r_int_begin(val);
    for (r_ssize j = 0; j < n_val; ++j) {
      v_val[j] = 1;
    }

    r_obj* dummy = create_dummy(val, pos, out_len, out_default);
    FREE(1);

    r_list_poke(out, k, dummy);
  }

  FREE(2);

  return out;
}

static r_obj* sparse_dummy_na(
  const int* v_x,
  r_ssize len,
  unsigned int shift,
  r_ssize n_lvls,
  const int* v_counts,
  r_ssize n_na,
  r_obj* out_len,
  r_obj* out_default
) {
  r_ssize n_own = 0;
  for (r_ssize k = 0; k < n_lvls; ++k) {
    n_own += v_counts[k];
  }

  r_obj* na_pos = KEEP(r_alloc_integer(n_na + 1));
  int* v_na_pos = r_int_begin(na_pos);

  r_obj* own = KEEP(r_alloc_integer(2 * (n_own + 1)));
  int* v_own = r_int_begin(own);

  r_obj* cursors = KEEP(r_alloc_integer(n_lvls + 1));
  int* v_cursors = r_int_begin(cursors);

  int offset = 0;
  for (r_ssize k = 0; k < n_lvls; ++k) {
    v_cursors[k] = offset;
    offset += v_counts[k];
  }
  v_cursors[n_lvls] = offset;

  const unsigned int n_lvls_uint = (unsigned int) n_lvls;
  int n_seen = 0;

  for (r_ssize i = 0; i < len; ++i) {
    const int code = v_x[i];
    const unsigned int k = (unsigned int) code - shift;
    const unsigned int slot = k < n_lvls_uint ? k : n_lvls_uint;

    int* v_e = v_own + 2 * (r_ssize) v_cursors[slot];

    // we need the result to be 1-indexed
    v_e[0] = i + 1;
    v_e[1] = n_seen;
    v_cursors[slot] += (slot < n_lvls_uint);

    v_na_pos[n_seen] = i + 1;
    n_seen += (code == r_globals.na_int);
  }

  r_obj* out = KEEP(r_alloc_list(n_lvls));

  r_ssize first = 0;
  for (r_ssize k = 0; k < n_lvls; ++k) {
    const r_ssize n_k = v_counts[k];
    const r_ssize n_val = n_k + n_na;

    r_obj* val = KEEP(r_alloc_integer(n_val));
    int* v_val = r_int_begin(val);
    r_obj* pos = KEEP(r_alloc_integer(n_val));
    int* v_pos = r_int_begin(pos);

    r_ssize n_out = 0;
    r_ssize n_used = 0;

    for (r_ssize j = first; j < first + n_k; ++j) {
      const int* v_e = v_own + 2 * j;
      const r_ssize n_before = v_e[1];
      const r_ssize n_gap = n_before - n_used;

      sparse_dummy_fill_na(
        v_pos + n_out,
        v_val + n_out,
        v_na_pos + n_used,
        n_gap
      );
      n_out += n_gap;
      n_used = n_before;

      v_pos[n_out] = v_e[0];
      v_val[n_out] = 1;
      ++n_out;
    }

    sparse_dummy_fill_na(
      v_pos + n_out,
      v_val + n_out,
      v_na_pos + n_used,
      n_na - n_used
    );

    first += n_k;

    r_obj* dummy = create_dummy(val, pos, out_len, out_default);
    FREE(2);

    r_list_poke(out, k, dummy);
  }

  FREE(4);

  return out;
}

static inline void sparse_dummy_fill_na(
  int* v_pos,
  int* v_val,
  const int* v_na_pos,
  r_ssize n
) {
  const int na = r_globals.na_int;

  for (r_ssize i = 0; i < n; ++i) {
    v_pos[i] = v_na_pos[i];
    v_val[i] = na;
  }
}

static r_obj* create_dummy(r_obj* val, r_obj* pos, r_obj* len, r_obj* def) {
  r_obj* out = KEEP(r_alloc_list(4));

  r_list_poke(out, 0, val);
  r_list_poke(out, 1, pos);
  r_list_poke(out, 2, len);
  r_list_poke(out, 3, def);

  r_obj* altrep = new_altrep_sparse_integer(out);

  FREE(1);

  return altrep;
}
