#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP SpaDES_pointDistance2(SEXP, SEXP);
extern SEXP SpaDES_pointDistance3(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP SpaDES_duplicatedInt(SEXP);
extern SEXP SpaDES_runifC(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"SpaDES_pointDistance2", (DL_FUNC) &SpaDES_pointDistance2, 2},
    {"SpaDES_pointDistance3", (DL_FUNC) &SpaDES_pointDistance3, 5},
    {"SpaDES_duplicatedInt", (DL_FUNC) &SpaDES_duplicatedInt, 1},
    {"SpaDES_runifC", (DL_FUNC) &SpaDES_runifC, 1},
    {NULL, NULL, 0}
};

void R_init_SpaDES(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

