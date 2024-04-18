#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Altrep.h>

static R_altrep_class_t sparsevctrs_real_class;

static R_xlen_t sparsevctrs_Length(SEXP sx)
{
    double res = 32;
    return res;
}

static void *sparsevctrs_Dataptr(SEXP sx, Rboolean writeable)
{
    return sx;
}

static const void *sparsevctrs_Dataptr_or_null(SEXP sx)
{
    return sx;
}

static double sparsevctrs_real_Elt(SEXP sx, R_xlen_t i)
{
    return i;
}

static
R_xlen_t sparsevctrs_real_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    return n;
}

static void InitRealClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altreal_class("sparsevctrs_real", "sparsevctrs", dll);
    sparsevctrs_real_class = cls;

    R_set_altrep_Length_method(cls, sparsevctrs_Length);

    R_set_altvec_Dataptr_method(cls, sparsevctrs_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, sparsevctrs_Dataptr_or_null);

    R_set_altreal_Elt_method(cls, sparsevctrs_real_Elt);
    R_set_altreal_Get_region_method(cls, sparsevctrs_real_Get_region);
}

SEXP do_sparsevctrs(SEXP sWhat) {
    return R_new_altrep(sparsevctrs_real_class, sWhat, R_NilValue);
}

static const R_CallMethodDef CallEntries[] = {
    {"sparsevctrs", (DL_FUNC) &do_sparsevctrs, -1},
    {NULL, NULL, 0}
};

void R_init_sparsevctrs(DllInfo *dll)
{
    InitRealClass(dll);

    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
