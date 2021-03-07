
##' Read a file in a different encoding and return it as UTF-8 using iconv
##'
##' The function is an adapted version of the one in the Rcpp Gallery post at
##' \url{https://gallery.rcpp.org/articles/iconv-via-r-header/} which is itself based on
##' \url{https://fishandwhistle.net/post/2021/using-rs-cross-platform-iconv-wrapper-from-cpp11/}.
##' @title readAndConvert
##' @param filename Character variable with path a file with text in encoding
##' @param encoding Optional character variable with the encoding, if unset via the default
##' empty string value no conversion is attempted.
##' @return A character variable with converted file content.
readAndConvert <- function(filename, encoding="") {
    stopifnot(`R not built with iconv support`=capabilities("iconv"))
    .Call("_readAndConvert", filename, encoding)
}
