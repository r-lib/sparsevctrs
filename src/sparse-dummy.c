#include "sparsevctrs-core.h"

#include "sparse-dummy.h"
#include "altrep-sparse-integer.h"

#include "decl/sparse-dummy-decl.h"

r_obj* ffi_sparse_dummy(
  r_obj* ffi_x,
  r_obj* ffi_lvls,
  r_obj* ffi_counts,
  r_obj* ffi_one_hot
) {
  const r_ssize n_lvls = r_length(ffi_lvls);
  const r_ssize len = r_length(ffi_x);

  const int* v_x = r_int_cbegin(ffi_x);
  const int* v_counts = r_int_cbegin(ffi_counts);
  const bool one_hot = r_lgl_get(ffi_one_hot, 0) == 1;

  // Generate list of integer vectors. One vector for each level, with its
  // length equal to the number of occurances of that level.
  r_obj* out = KEEP(r_alloc_list(n_lvls));

  for (r_ssize i = 0; i < n_lvls; ++i) {
    const r_ssize n_val = v_counts[i];
    r_list_poke(out, i, r_alloc_integer(n_val));
  }

  // Vector of positions to keep track of how far into each position vector we
  // are. Initialize to 0 to indicate first position.
  r_obj* pos_index = KEEP(r_alloc_integer(n_lvls));
  int* v_pos_index = r_int_begin(pos_index);
  for (r_ssize i = 0; i < n_lvls; ++i) {
    v_pos_index[i] = 0;
  }

  // Itterate over input, find its position index, and place the position value
  // at the index. Increment specific index.
  if (one_hot) {
    for (r_ssize i = 0; i < len; ++i) {
      const int current_val = v_x[i] - 1;

      const int index = v_pos_index[current_val];

      r_obj* pos_vec = r_list_get(out, current_val);
      int* v_pos_vec = r_int_begin(pos_vec);

      // we need the result to be 1-indexed
      v_pos_vec[index] = i + 1;
      v_pos_index[current_val]++;
    }
  } else {
    for (r_ssize i = 0; i < len; ++i) {
      const int current_val = v_x[i] - 1;

      if (current_val == -1) {
        continue;
      }

      const int index = v_pos_index[current_val];

      r_obj* pos_vec = r_list_get(out, current_val);
      int* v_pos_vec = r_int_begin(pos_vec);

      // we need the result to be 1-indexed
      v_pos_vec[index] = i + 1;
      v_pos_index[current_val]++;
    }
  }

  // Turn list of integer vectors with positions, into list of sparse integer
  // vectors.
  for (r_ssize i = 0; i < n_lvls; ++i) {
    r_obj* positions = r_list_get(out, i);
    r_obj* dummy = create_dummy(positions, len);
    r_list_poke(out, i, dummy);
  }

  FREE(2);

  return out;
}

r_obj* ffi_sparse_dummy_na(
  r_obj* ffi_x,
  r_obj* ffi_lvls,
  r_obj* ffi_counts,
  r_obj* ffi_one_hot
) {
  const r_ssize n_lvls = r_length(ffi_lvls);
  const r_ssize len = r_length(ffi_x);

  const int* v_x = r_int_cbegin(ffi_x);
  const int* v_counts = r_int_cbegin(ffi_counts);
  const bool one_hot = r_lgl_get(ffi_one_hot, 0) == 1;

  // Generate lists of integer vectors. One vector for each level, with its
  // length equal to the number of occurances of that level.
  r_obj* out_positions = KEEP(r_alloc_list(n_lvls));
  r_obj* out_values = KEEP(r_alloc_list(n_lvls));

  for (r_ssize i = 0; i < n_lvls; ++i) {
    const r_ssize n_val = v_counts[i];
    r_list_poke(out_values, i, r_alloc_integer(n_val));
    r_list_poke(out_positions, i, r_alloc_integer(n_val));
  }

  // Vector of positions to keep track of how far into each position vector we
  // are. Initialize to 0 to indicate first position.
  r_obj* pos_index = KEEP(r_alloc_integer(n_lvls));
  int* v_pos_index = r_int_begin(pos_index);
  for (r_ssize i = 0; i < n_lvls; ++i) {
    v_pos_index[i] = 0;
  }

  // Itterate over input, find its position index, and place the position value
  // at the index. Increment specific index.

  if (one_hot) {
    for (r_ssize i = 0; i < len; ++i) {
      int current_val = v_x[i];

      if (current_val == r_globals.na_int) {
        for (r_ssize j = 0; j < n_lvls; ++j) {
          const int index = v_pos_index[j];

          r_obj* pos_vec = r_list_get(out_positions, j);
          int* v_pos_vec = r_int_begin(pos_vec);
          r_obj* val_vec = r_list_get(out_values, j);
          int* v_val_vec = r_int_begin(val_vec);

          v_pos_vec[index] = i + 1;
          v_val_vec[index] = r_globals.na_int;
          v_pos_index[j]++;
        }
      } else {
        --current_val;
        const int index = v_pos_index[current_val];

        r_obj* pos_vec = r_list_get(out_positions, current_val);
        int* v_pos_vec = r_int_begin(pos_vec);
        r_obj* val_vec = r_list_get(out_values, current_val);
        int* v_val_vec = r_int_begin(val_vec);

        // we need the result to be 1-indexed
        v_pos_vec[index] = i + 1;
        v_val_vec[index] = 1;
        v_pos_index[current_val]++;
      }
    }
  } else {
    for (r_ssize i = 0; i < len; ++i) {
      int current_val = v_x[i];

      if (current_val == r_globals.na_int) {
        for (r_ssize j = 0; j < n_lvls; ++j) {
          const int index = v_pos_index[j];

          r_obj* pos_vec = r_list_get(out_positions, j);
          int* v_pos_vec = r_int_begin(pos_vec);
          r_obj* val_vec = r_list_get(out_values, j);
          int* v_val_vec = r_int_begin(val_vec);

          v_pos_vec[index] = i + 1;
          v_val_vec[index] = r_globals.na_int;
          v_pos_index[j]++;
        }
      } else {
        --current_val;
        if (current_val == -1) {
          continue;
        }

        const int index = v_pos_index[current_val];

        r_obj* pos_vec = r_list_get(out_positions, current_val);
        int* v_pos_vec = r_int_begin(pos_vec);
        r_obj* val_vec = r_list_get(out_values, current_val);
        int* v_val_vec = r_int_begin(val_vec);

        // we need the result to be 1-indexed
        v_pos_vec[index] = i + 1;
        v_val_vec[index] = 1;
        v_pos_index[current_val]++;
      }
    }
  }

  // Turn list of integer vectors with positions, into list of sparse integer
  // vectors.
  for (r_ssize i = 0; i < n_lvls; ++i) {
    r_obj* positions = r_list_get(out_positions, i);
    r_obj* values = r_list_get(out_values, i);
    r_obj* dummy = create_dummy_na(values, positions, len);
    r_list_poke(out_positions, i, dummy);
  }

  FREE(3);

  return out_positions;
}

static r_obj* create_dummy(r_obj* pos, r_ssize length) {
  r_obj* out = KEEP(r_alloc_list(4));

  const r_ssize pos_len = r_length(pos);

  // values
  r_obj* out_val = r_alloc_integer(pos_len);
  r_list_poke(out, 0, out_val);
  int* v_out_val = r_int_begin(out_val);

  for (r_ssize i = 0; i < pos_len; ++i) {
    v_out_val[i] = 1;
  }

  // positions
  r_list_poke(out, 1, pos);

  // length
  r_obj* out_length = r_int((int) length);
  r_list_poke(out, 2, out_length);

  // default
  r_obj* out_default = r_int(0);
  r_list_poke(out, 3, out_default);

  r_obj* altrep = new_altrep_sparse_integer(out);

  FREE(1);

  return altrep;
}

static r_obj* create_dummy_na(r_obj* values, r_obj* pos, r_ssize length) {
  r_obj* out = KEEP(r_alloc_list(4));

  // values
  r_list_poke(out, 0, values);

  // positions
  r_list_poke(out, 1, pos);

  // length
  r_obj* out_length = r_int((int) length);
  r_list_poke(out, 2, out_length);

  // default
  r_obj* out_default = r_int(0);
  r_list_poke(out, 3, out_default);

  r_obj* altrep = new_altrep_sparse_integer(out);

  FREE(1);

  return altrep;
}
