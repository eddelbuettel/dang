## source: https://gist.github.com/brodieG/e60c94d4036f45018530ea504258bcf3#file-cran-check-r
## author: Brodie Gaslam

## Made cache file be inside tempdir
## And imposed snakeCase because better :)

# started with `foghorn`, but that comes with dependencies and
# slows down R startup.  `browseURL` just barely slows startup,
# even if actual page is slow to load.

## check_cran_old <- function(email) {
##     utils::browseURL(
##                sprintf(
##                    "https://cran.r-project.org/web/checks/check_results_%s.html",
##                    gsub("[^A-Za-z0-9_:.-]", "_", sub("@", "_at_", email))
##                ) ) }
## if(interactive()) check_cran_old("edd@debian.org")

# an alternative that is not as disruptive as `browserURL`.
# The regex is likely to be pretty fragile to changes in
# page structure, but the benefit is no dependencies:

##' Report Maintainer Status at CRAN
##'
##' This function retrieves the maintainer status (given an email)
##' at CRAN. Values are optionally cached; the default cache location
##' is inside the per-session temporary directory as R should not write
##' elsewhere.
##' @title Maintainer Status at CRAN
##' @param email A character variable with the maintainer email
##' @param cache A character variable with an optional cache file,
##' default value is to use a file inside the per-session
##' temporary directory
##' @param cache.life A numeric timeout, defaults to one day
##' @return Nothing, the sideffect of the display is the main effect
##' @author Brodie Gaslam (with minor modifications by Dirk Eddelbuettel)
checkCRANStatus <- function(email, cache, cache.life=24 * 3600) {

    if (missing(cache)) cache <- tempfile(pattern="cran-status", fileext=".rds")

    url <- sprintf("https://cran.r-project.org/web/checks/check_results_%s.html",
                   gsub("[^A-Za-z0-9_:.-]", "_", sub("@", "_at_", tolower(email))))

    display_check <- function(x, extra=NULL) {
        print(x)
        err.cols <- unlist(x[names(x) %in% c("WARNING", "ERROR")])
        if (sum(as.numeric(err.cols), na.rm=TRUE))
            writeLines(c("\033[41mErrors/Warnings Present\033[m", url))
        writeLines(c(extra, ""))
    }
    renew.cache <- TRUE
    if (file.exists(cache)) {
        cache.dat <- readRDS(cache)
        cache.age <- Sys.time() - cache.dat[[1]]
        if (as.double(cache.age, 'secs') < cache.life) {
            renew.cache <- FALSE
            display_check(cache.dat[[2]],
                          c("", sprintf("cached CRAN status (%s old).", format(round(cache.age))) ) )
        }
    }
    if (renew.cache) {
        cat("connecting to CRAN...")
        page <- readLines(url)
        cat("\r                     \r")
        pattern <- "\\s*<t[hd].*?>(.*?)</t[hd]>"
        has.rows <- grep(pattern, page, perl=TRUE)
        strings <- gregexpr(pattern, page[has.rows], perl=TRUE)
        res <- sapply(regmatches(page[has.rows], strings),
                      function(x) {
            submatch <- regexec(pattern, x, perl=TRUE)
            vapply(regmatches(x, submatch), "[[", character(1L), 2)
        })
        res.mx <- t(gsub("<[^>]*>|^\\s+|\\s+$", "", res))
        res.mx.2 <- res.mx[-1, , drop = FALSE]
        colnames(res.mx.2) <- res.mx[1, ]
        res.df <- as.data.frame(res.mx.2, stringsAsFactors=FALSE)
        saveRDS(list(Sys.time(), res.df), cache)
        display_check(res.df)
    }
}
#if (interactive()) check_cran('edd@debian.org')
