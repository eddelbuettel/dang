#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _check_nonASCII(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_check_nonASCII", (DL_FUNC) &_check_nonASCII, 2},
    {NULL, NULL, 0}
};

void R_init_dang(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
