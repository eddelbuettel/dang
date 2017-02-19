##' Convert an xts object into a data.table
##'
##' This is still experimental.  Note that all four \emph{added}
##' columns are keys to the data.table object, and that
##' \code{setNumericRounding(0)} is executed too,
##' @title Convert an xts object into a data.table
##' @param x An xts object
##' @return A data.table object with new columns date, time, micros,
##'  pt providing, respectively the data as \code{IDate}, time as
##' \code{ITime}, microseconds (rounded) and numeric
##' \code{POSIXct}.
##' @author Dirk Eddelbuettel
as.data.table.xts <- function(x) {
    if (requireNamespace("zoo", quietly=TRUE) && requireNamespace("data.table", quietly=TRUE)) {
        pt <- time <- micros <- NULL        # to make R CMD check happy
        dt <-  data.table::data.table(date = data.table::as.IDate(zoo::index(x)),
                                      time = data.table::as.ITime(zoo::index(x)),
                                      micros = round(1e6*(as.numeric(zoo::index(x) - trunc(zoo::index(x))))),
                                      pt = as.numeric(zoo::index(x)),
                          zoo::coredata(x))
        data.table::setNumericRounding(0)
        data.table::setkey(dt, pt, date, time, micros)
        dt
    } else {
        stop("Cannot covert to 'data.table' without the 'zoo' package/", call.=FALSE)
    }
}
