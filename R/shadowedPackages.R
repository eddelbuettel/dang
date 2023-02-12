##' Return a data.table object with \sQuote{shadowed} packages,
##' meaning package which are installed in more than directory on the
##' \code{.libPaths}.
##'
##' The function relies on the base R functions
##' \code{installed.packages()} to find all packages, as well as the
##' base R abilility to compare version numbers (once properly
##' converted to \code{package_version} type).
##' @title Find Shadowed Packages
##' @return data.table object with packages
##' @author Dirk Eddelbuettel
shadowedPackages <- function() {
    if (!requireNamespace("data.table", quietly=TRUE)) {
        message("Please install data.table")
        return(invisible())
    }
    require(data.table)
    ip <- installed.packages()
    d <- data.table(ip[,1:3])
    d <- d[, Version := as.package_version(Version)]
    d <- d[, n := .N, by = Package]
    d[n > 1, good := Version == max(Version), by = Package][n > 1,]
}
