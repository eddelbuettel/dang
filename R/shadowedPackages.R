##' Return a data.frame object with \sQuote{shadowed} packages,
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
    ## if (!requireNamespace("data.table", quietly=TRUE)) {
    ##     message("Please install data.table")
    ##     return(invisible())
    ## }
    ## require(data.table)
    ## ip <- installed.packages()
    ## d <- data.table(ip[,1:3])
    ## d <- d[, Version := as.package_version(Version)]
    ## d <- d[, n := .N, by = Package]
    ## d[n > 1, good := Version == max(Version), by = Package][n > 1,]

    ## base R version with a tip-of-the-hat to Vincent Arel-Bundock
    ip <- installed.packages()
    d <- as.data.frame(ip[, 1:3])
    d$Version <- as.package_version(d$Version)
    d <- d[duplicated(d$Package) | duplicated(d$Package, fromLast = TRUE), , drop = FALSE]
    d <- by(d, d$Package, FUN = function(x) transform(x, Latest = Version == max(Version)))
    d <- do.call(rbind, d)
    rownames(d) <- NULL
    return(d)
}
