#ifndef SPARSEVCTRS_CORE_H
#define SPARSEVCTRS_CORE_H

#include <rlang.h>
#include <R_ext/Altrep.h>
#include <Rversion.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>

// Backport DATAPTR_RW for R < 4.6.0 (as recommended in Writing R Extensions)
#if R_VERSION < R_Version(4, 6, 0)
#define DATAPTR_RW(x) DATAPTR(x)
#endif

// Backport R_altrep_class_name and R_altrep_class_package for R < 4.6.0
#if R_VERSION < R_Version(4, 6, 0)
static inline SEXP R_altrep_class_name(SEXP x) {
  return ALTREP(x) ? CAR(ATTRIB(ALTREP_CLASS(x))) : R_NilValue;
}
static inline SEXP R_altrep_class_package(SEXP x) {
  return ALTREP(x) ? CADR(ATTRIB(ALTREP_CLASS(x))) : R_NilValue;
}
#endif

#endif
