## R package development

### Key commands

```
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests
Rscript -e "devtools::test()"

# To run all tests for files starting with {name}
Rscript -e "devtools::test(filter = '^{name}')"

# To run all tests for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R')"

# To run a single test "blah" for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R', desc = 'blah')"

# To redocument the package
Rscript -e "devtools::document()"

# To check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# To check the package with R CMD check
Rscript -e "devtools::check()"

# To format code
air format .
```

### Coding

* Always run `air format .` after generating code
* Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`)
* Don't use `_$x` or `_$[["x"]]` since this package must work on R 4.1.
* Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}` 

### C code

- Prefer the code style of vctrs and rlang, particularly for the C code. The sources are at https://github.com/r-lib/vctrs and https://github.com/r-lib/rlang, treat any local clone as read only.
- For checking how R itself implements something in C, read the headers in `$(R RHOME)/include` or the R sources at https://github.com/wch/r-source. Treat any local copy as read only.
- Each feature gets a `src/{name}.c` and `src/{name}.h` pair.
- Internal C functions: `sparse_{name}()`, returning C types (e.g., `r_ssize`).
- FFI wrappers: `ffi_sparse_{name}()`, thin SEXP-to-C bridges. Go above internal functions in the `.c` file.
- FFI wrapper parameters are also prefixed, i.e. `ffi_sparse_foo(r_obj* ffi_x, r_obj* ffi_y)`. This frees up the unprefixed name for the converted C value.
- Order a `.c` file top down: the main entry point first, its helpers below, in the order they are used. Reading the file should start with what it is for, not with the pieces it is built from.
- `src/decl/{name}-decl.h` exists to make that ordering work. Declare every helper there so the `.c` file never needs a forward declaration of its own.
- A decl header has no includes and no include guard. It is included last, from exactly one `.c` file, so everything it names is already in scope and there is nothing to guard against.
- `#include "decl/{name}-decl.h"` always goes last, after every other include, separated from them by one blank line.
- Headers only declare internal C functions, not FFI wrappers.
- `src/init.c` uses `extern` declarations for FFI functions, it does not include feature headers.
- Always prefer rlang's C library wrappers over the raw R API (e.g., `r_globals.na_int` over `NA_INTEGER`, `r_length()` over `Rf_length()`).
- Prefer `r_ssize` over `int`, `r_length()` over `Rf_length()`, and `r_attrib_get()` plus `r_null` checks over `Rf_isArray()`.
- When looping over a vector, get a pointer to the underlying data first, e.g. `const int* v_x = r_int_cbegin(x)`, then index into that directly.
- Mark variables as `const` where possible.
- Any `r_obj*` returned by a C function that allocates must be protected with `KEEP()` / `FREE()` (rlang's wrappers around `PROTECT()` / `UNPROTECT()`) if used after any further allocation could occur.
- Before calling any C change done: for every new or touched `r_obj*` in the diff, name the function it is next passed to or read by, and check whether that function allocates before it protects or consumes the value. A value is not safe just because it looks used immediately, the callee's own allocations count. Do this as an explicit, separate pass over the diff, the same way `clang-format` is a separate mandatory step.
- Never use `gctorture()` or `gctorture2()` to look for protection problems. They are far too slow to be practical. The reading pass described above is the check.
- Never change code inside of `src/rlang/`, that is a vendored library.
- Always run `clang-format -i src/*.c src/*.h` after generating C code. Always run it over all files, not just changed files. If `clang-format` is not on the PATH, use `/opt/homebrew/opt/llvm/bin/clang-format`.

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`. 
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep your tests minimal with few comments.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.
- Whenever you add a new (non-internal) documentation topic, also add the topic to `_pkgdown.yml`. 
- Always re-document the package after changing a roxygen2 comment.
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.

### GitHub

- If you use `gh` to retrieve information about an issue, always use `--comments` to read all the comments.

### Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and editor with a deep understanding of clear, engaging, and well-structured writing. 

Work paragraph by paragraph, always starting by making a TODO list that includes individual items for each top-level heading. 

Fix spelling, grammar, and other minor problems without asking the user. Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.
