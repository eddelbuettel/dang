
##' Rolling Minimum or Maximum over a Fixed Window
##'
##' This implementation is minimal without error checking, or NA handling.
##' It is taken from the \pkg{ichimoku} package which had several more
##' complicated variants, and is reused here with just \pkg{tidyCpp}.
##'
##' The \pkg{ichimoku} variant is by Charlie Gao and credits Andrew Uhl for
##' the initial implementation.
##'
##' @param x A numeric vector.
##' @param window An interger with the size of the rolling window.
##' @param min A logical which, if true, selects minimum, else maximum
##' @return A vector of the same length as 'x' with elements 1 to
##' (length(window) - 1) containing NAs.
##' @author Dirk Eddelbuettel for this version, R Core for the underlying code
rollMinMax <- function(x, window, min=TRUE) {
    .Call("_rollMinMax", x, window, min)
}
