
// This file is part of file src/library/tools/src/text.c in the R sources e.g.
//   https://github.com/wch/r-source/blob/trunk/src/library/tools/src/text.c
// and has been modified / simplified to use the tidyCpp API wrapper package
//
// Those modifications are Copyright (c) 2020 - present Dirk Eddelbuettel
// and released as usual under GPL (>= 2) as well.  The original is by R Core.


//  R : A Computer Language for Statistical Data Analysis
//  Copyright (C) 2003-2016   The R Core Team.
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, a copy is available at
//  https://www.R-project.org/Licenses/
//

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
                    return R::scalarLogical(TRUE);
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
    return R::scalarLogical(FALSE);
}

} // extern "C"
