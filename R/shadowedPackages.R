##' Return a data.table object with \sQuote{shadowed} packages,
##' meaning package which are installed in more than directory on the
##' \code{.libPaths}.
##'
##' The function relies on the base R functions
##' \code{installed.packages()} to find all packages, as well as the
##' base R abilility to compare version numbers (once properly
##' converted to \code{package_version} type).
##' @title Find Shadowed Packages
##' @return data.table object with packages that are shadowed
##' @author Dirk Eddelbuettel
shadowedPackages <- function() {
    if (!requireNamespace("data.table", quietly=TRUE)) {
        message("Please install data.table")
        return(invisible())
    }
    ip <- installed.packages()
    d <- data.table::data.table(ip[,1:3])
    d[, Version := as.package_version(Version)]
    d[, n := .N, keyby = Package]
    d[n > 1, good := Version == max(Version), by = Package]
    d[, Version := format(Version)]   # data.table 1.14.6 prefers this
    d[n > 1,]
}
## Ensure the `[` dispatches to data.table
.datatable.aware <- TRUE

utils::globalVariables(c(".N", "Package", "Version", "good", "n"))
