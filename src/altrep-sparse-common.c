#include "sparsevctrs-core.h"

#include "altrep-sparse-common.h"

r_obj* sparse_new(
  r_obj* val,
  r_obj* pos,
  r_obj* len,
  r_obj* def,
  sparse_new_altrep_fn new_altrep
) {
  r_obj* list = KEEP(r_alloc_list(4));

  r_list_poke(list, 0, val);
  r_list_poke(list, 1, pos);
  r_list_poke(list, 2, len);
  r_list_poke(list, 3, def);

  r_obj* out = new_altrep(list);

  FREE(1);
  return out;
}

const void* sparse_dataptr_or_null(r_obj* x) {
  r_obj* out = R_altrep_data2(x);

  if (out == r_null) {
    return NULL;
  } else {
    return DATAPTR_RW(out);
  }
}

Rboolean sparse_inspect(r_obj* x, const char* class_name) {
  Rprintf(
    "sparsevctrs_%s (materialized=%s, length=%i)\n",
    class_name,
    R_altrep_data2(x) != r_null ? "T" : "F",
    (int) sparse_extract_len(x)
  );
  return TRUE;
}

r_obj* sparse_duplicate(
  r_obj* x,
  Rboolean deep,
  sparse_new_altrep_fn new_altrep
) {
  r_obj* data1 = R_altrep_data1(x);
  r_obj* data2 = R_altrep_data2(x);

  /* If deep or already materialized, do the default behavior */
  if (deep || data2 != r_null) {
    return NULL;
  }

  return new_altrep(data1);
}

r_obj* sparse_subset_matches(r_obj* x, r_obj* indx, r_ssize* p_n_hits) {
  const r_ssize len = sparse_extract_len(x);

  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);
  const r_ssize n_pos = r_length(pos);

  const int* v_indx = r_int_cbegin(indx);

  const r_ssize size = r_length(indx);

  r_ssize n_hits = 0;
  r_obj* matches = r_alloc_integer(size);
  int* v_matches = r_int_begin(matches);

  for (r_ssize i = 0; i < size; ++i) {
    // 1 indexed!
    const int index = v_indx[i];

    if (index == r_globals.na_int) {
      v_matches[i] = r_globals.na_int;
      ++n_hits;
      continue;
    }

    if (index > len) {
      // (Uses `>` not `>=` because `index` is 1 indexed)
      // OOB
      v_matches[i] = r_globals.na_int;
      ++n_hits;
      continue;
    }

    const r_ssize loc = sparse_binary_search(index, v_pos, n_pos);

    if (loc == n_pos) {
      // Not in `pos`, gets default value
      v_matches[i] = (int) n_pos;
      continue;
    }

    // Did find in `pos`
    v_matches[i] = (int) loc;
    ++n_hits;
  }

  *p_n_hits = n_hits;
  return matches;
}

r_obj* sparse_subset_list(r_obj* x, r_ssize size, r_obj* out_val) {
  r_obj* out = KEEP(r_alloc_list(4));

  r_list_poke(out, 0, out_val);

  r_obj* out_pos = r_alloc_integer(r_length(out_val));
  r_list_poke(out, 1, out_pos);

  r_obj* out_length = r_int((int) size);
  r_list_poke(out, 2, out_length);

  r_obj* out_default = sparse_extract_default(x);
  r_list_poke(out, 3, out_default);

  FREE(1);
  return out;
}

int sparse_int_is_sorted(r_obj* x, const int* v_val, int v_default_val) {
  r_obj* pos = sparse_extract_pos(x);
  const int* v_pos = r_int_cbegin(pos);

  const r_ssize pos_len = r_length(pos);

  // zero length vector are by def sorted
  if (pos_len == 0) {
    return TRUE;
  }

  int current_value;

  if (v_pos[0] == 1) {
    current_value = v_val[0];
  } else {
    current_value = v_default_val;
  }

  for (r_ssize i = 0; i < pos_len; i++) {
    if (v_val[i] == r_globals.na_int) {
      return FALSE;
    }

    if (v_val[i] < current_value) {
      return FALSE;
    }

    current_value = v_val[i];

    if (i + 1 == pos_len) {
      break;
    }

    // If there is a gap between values check against default
    if ((v_pos[i + 1] - v_pos[i]) > 1) {
      if (v_default_val < current_value) {
        return FALSE;
      }

      current_value = v_default_val;
    }
  }

  // Trailing default values after the last stored value
  if (v_pos[pos_len - 1] < sparse_extract_len(x)) {
    if (v_default_val < current_value) {
      return FALSE;
    }
  }

  return TRUE;
}

int sparse_int_no_na(r_obj* x, const int* v_val) {
  const r_ssize val_len = r_length(sparse_extract_val(x));

  for (r_ssize i = 0; i < val_len; i++) {
    if (v_val[i] == r_globals.na_int) {
      return FALSE;
    }
  }

  return TRUE;
}
