
// This is a modified version of the file
//    https://github.com/shikokuchuo/ichimoku/blob/6462c5ee65642f8018bd51ae529499c705a15c95/src/windowfns.cpp
// (where we reference an older commit).  This file itself lists the following header

// This file is modified from the original with the following license:
/*
 Based on http://opensource.org/licenses/MIT
 Copyright (c) 2015, Andrew Uhl
 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and associated documentation files (the
 "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */


// It has been modified to work with tidyCpp
// Those modifications are Copyright (c) 2021 - present Dirk Eddelbuettel
// and released as usual under GPL (>= 2). The ichimoku package is GPL-3


#include <deque>
#include <utility>
#include <tidyCpp>


// Note that NumVec is a trivial class wrapping SEXP inside tidyCpp
// But in order to provide a clean C interface we wrap an outer SEXP
// function around it we can refer from C without reference to C++ types

// Calculates rolling window for {minimum, maximum}
tidy::NumVec rollMinMax(tidy::NumVec x, int window, bool isMin=TRUE) {

    int n  = R::length(x);
    tidy::NumVec rollx(n);

    std::deque< std::pair<long double, int> > deck;
    for (int i = 0; i < n; ++i) {
        double xv = x[i];
        if (isMin) {
            while (!deck.empty() && deck.back().first >= xv)
                deck.pop_back();
        } else {
            while (!deck.empty() && deck.back().first <= xv)
                deck.pop_back();
        }
        deck.push_back(std::make_pair(xv, i));

        while(deck.front().second <= i - window)
            deck.pop_front();

        long double min = deck.front().first;
        if (i < window - 1) {
            rollx[i] = NA_REAL;
        } else {
            rollx[i] = min;
        }
    }
    return rollx;
}

extern "C" {

    // this SEXP variant is referenced from init.c and callable from R
    SEXP _rollMinMax(SEXP x, SEXP window, SEXP isMin) {
        return rollMinMax(x, R::asInteger(window), R::asLogical(isMin));
    }

}
