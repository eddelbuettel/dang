#include <tidyCpp>

extern "C" {

// cf src/library/tools/src/text.c
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

    if (!R::isString(text)) R::error("invalid input");
    ign = (Rboolean) R::asLogical(ignore_quotes);
    if (ign == NA_LOGICAL) R::error("'ignore_quotes' must be TRUE or FALSE");

    for (i = 0; i < R::length(text); i++) {
        p = R::charPointer(R::stringElement(text, i)); // ASCII or not not affected by charset
	inquote = FALSE; /* avoid runaway quotes */
	for (; *p; p++) {
	    if (!inquote && *p == '#') break;
	    if (!inquote || ign) {
		if ((unsigned int) *p > 127) {
                    Rprintf("%s\n", R::charPointer(R::stringElement(text, i)));
		    Rprintf("found %x\n", (unsigned int) *p);
		    return R::ScalarLogical(TRUE);
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
    return R::ScalarLogical(FALSE);
}

} // extern "C"
