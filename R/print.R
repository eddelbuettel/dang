##' Print helper function for xts objects.
##'
##' @title Printing helper function for xts object
##' @param x An xts objects
##' @param ... Ignored, but needed for consistency with \code{print}
##'  generic.
##' @param n The default number of lines to display, default is 10.
##' @return The return from \code{print}
##' @author Dirk Eddelbuettel
print.xts <- function(x, ..., n = 10) {
    if (nrow(x) <= 10) {
        print(as.data.frame(x))
    } else {
        print(head(as.data.frame(x),n))
        cat("...\n")
        print(tail(as.data.frame(x),n))
    }
}
