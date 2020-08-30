#include <R.h>
#include <Rdefines.h>

extern "C" {

SEXP _check_nonASCII(SEXP text, SEXP ignore_quotes) {
    /* Check if all the lines in 'text' are ASCII, after removing
       comments and ignoring the contents of quotes (unless ignore_quotes)
       (which might span more than one line and might be escaped).

       This cannot be entirely correct, as quotes and \ might occur as
       part of another character in a MBCS: but this does not happen
       in UTF-8.
    */
    int i, nbslash = 0; /* number of preceding backslashes */
    const char *p;
    char quote= '\0';
    Rboolean ign, inquote = FALSE;

    if (TYPEOF(text) != STRSXP) Rf_error("invalid input");
    ign = (Rboolean)Rf_asLogical(ignore_quotes);
    if (ign == NA_LOGICAL) Rf_error("'ignore_quotes' must be TRUE or FALSE");

    for (i = 0; i < LENGTH(text); i++) {
	p = CHAR(STRING_ELT(text, i)); // ASCII or not not affected by charset
	inquote = FALSE; /* avoid runaway quotes */
	for (; *p; p++) {
	    if (!inquote && *p == '#') break;
	    if (!inquote || ign) {
		if ((unsigned int) *p > 127) {
		    Rprintf("%s\n", CHAR(STRING_ELT(text, i)));
		    Rprintf("found %x\n", (unsigned int) *p);
		    return Rf_ScalarLogical(TRUE);
		}
	    }
	    if ((nbslash % 2 == 0) && (*p == '"' || *p == '\'')) {
		if (inquote && *p == quote) {
		    inquote = FALSE;
		} else if(!inquote) {
		    quote = *p;
		    inquote = TRUE;
		}
	    }
	    if (*p == '\\') nbslash++; else nbslash = 0;
	}
    }
    return Rf_ScalarLogical(FALSE);
}
}
