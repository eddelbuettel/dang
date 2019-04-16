##' Function to (heuristically) test for a network connection by attempting
##' to connect to a given website.
##'
##' The main page of Google is used as a proxy for overall network
##' connectivity as Google is generally 'network-close' and the page
##' is relatively small.  If a network is unavailable this fails
##' generally already on domain name service resolution. Special thanks
##' to Barry and Brodie for a very helpful discussion re-discovering this
##' function.
##' @title Is the current session (networked) and connected?
##' @param site Character variable with site to try to connect to,
##' defauls to \code{http://www.google.com}
##' @return A boolean value indicating networking status
##' @author Dirk Eddelbuettel
isConnected <- function(site="https://www.google.com") {
    uoc <- function(site) {
        con <- url(site)                # need to assign so that we can close
        open(con)                       # in case of success we have a connection
        close(con)                      # ... so we need to clean up
    }
    suppressWarnings(!inherits(try(uoc(site), silent=TRUE), "try-error"))
}
