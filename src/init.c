#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _check_nonASCII(SEXP, SEXP);
extern SEXP _readAndConvert(SEXP, SEXP);
extern SEXP _rollMinMax(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_check_nonASCII", (DL_FUNC) &_check_nonASCII, 2},
    {"_readAndConvert", (DL_FUNC) &_readAndConvert, 2},
    {"_rollMinMax", (DL_FUNC) &_rollMinMax, 3},
    {NULL, NULL, 0}
};

void R_init_dang(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
