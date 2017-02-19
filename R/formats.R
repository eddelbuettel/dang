#' Format a Date(time) object as ymd
#'
#' @param pt A \code{POSIXt} Datetime or a \code{Date} object
#' @return A character object formatted as \sQuote{YYYYMMDD}
#' @author Dirk Eddelbuettel
#' @examples
#' if (requireNamespace("anytime", quietly=TRUE)) {
#'    ymd(anytime::anytime("2016-09-01 10:11:12.123456"))
#'    ymd(anytime::anydate("2016-Sep-01"))
#' }
ymd <- function(pt) {
    if (inherits(pt, "POSIXt"))
        return(format.POSIXct(pt, "%Y%m%d"))
    else if (inherits(pt, "Date"))
        return(format.Date(pt, "%Y%m%d"))

    warning("Inapplicable object: ", pt)
    invisible(NULL)
}

