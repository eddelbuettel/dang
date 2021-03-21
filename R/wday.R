
##' Return the day of the week as an integer
##'
##' This function simply wraps around the \code{as.POSIXlt} function and returns its
##' \code{wday} field.
##' @title wday
##' @param date A Date object, with the current date as the default
##' @return A integer with the weekday component of the date
wday <- function(date = Sys.Date()) {
    stopifnot(`Not a Date argument` = inherits(date, "Date"))
    unclass(as.POSIXlt(date))$wday
}
