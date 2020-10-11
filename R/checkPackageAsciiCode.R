
##' Check a package directory for non-ASCII characters in source files.
##'
##' The function is a renamed and slightly edited copy of the base R function
##' \code{.check_package_ASCII_code}. It uses an unexported C function, also
##' included, from base R, called as \code{_check_nonASCII}.
##' @title checkPackageAsciiCode
##' @param dir Character variable with path to directory to be checked
##' @param respect_quotes Logical variable whether quotes need to be checked
##' @return A vector of things that are wrong per this function, also displayed
##' on standard output
##' @author Dirk Eddelbuettel for this version, R Core for the underlying code
##' @examples
##' \dontrun{
##' checkPackageAsciiCode(".", FALSE)
##' }
checkPackageAsciiCode <- function(dir, respect_quotes = FALSE) {
    OS_subdirs <- c("unix", "windows")
    if (!dir.exists(dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- tools::file_path_as_absolute(dir)

    code_dir <- file.path(dir, "R")
    wrong_things <- character()
    if (dir.exists(code_dir)) {
        R_files <- tools::list_files_with_type(code_dir, "code", full.names = FALSE,
                                               OS_subdirs = OS_subdirs)
        for (f in R_files) {
            text <- readLines(file.path(code_dir, f), warn = FALSE)
            if (.Call("_check_nonASCII", text, !respect_quotes))
                wrong_things <- c(wrong_things, f)
        }
    }
    if (length(wrong_things)) cat(wrong_things, sep = "\n")
    invisible(wrong_things)
}
