##' Convert an xts object into a data.table
##'
##' This is still experimental.  Note that all four \emph{added}
##' columns are keys to the data.table object, and that
##' \code{setNumericRounding(0)} is executed too,
##' @title Convert an xts object into a data.table
##' @param x An xts object
##' @param ... Catch-all arguments passed on to methods
##' @return A data.table object with new columns date, time, micros,
##'  pt providing, respectively the data as \code{IDate}, time as
##' \code{ITime}, microseconds (rounded) and numeric
##' \code{POSIXct}.
##' @author Dirk Eddelbuettel
as.data.table.xts <- function(x) {
    if (!requireNamespace("zoo", quietly=TRUE) || !requireNamespace("data.table", quietly=TRUE)) {
        stop("Cannot covert to 'data.table' without the 'zoo' package.", call.=FALSE)
    }
    if (class(zoo::index(x)) == "Date") {
        date <- NULL
        dt <-  data.table::data.table(date = data.table::as.IDate(zoo::index(x)),
                                      zoo::coredata(x))
        data.table::setkey(dt, date)
        dt
    } else {
        pt <- time <- micros <- NULL        # to make R CMD check happy
        dt <-  data.table::data.table(date = data.table::as.IDate(zoo::index(x)),
                                      time = data.table::as.ITime(zoo::index(x)),
                                      micros = round(1e6*(as.numeric(zoo::index(x) - trunc(zoo::index(x))))),
                                      pt = as.numeric(zoo::index(x)),
                                      zoo::coredata(x))
        data.table::setNumericRounding(0)
        data.table::setkey(dt, pt, date, time, micros)
        dt
    }
}


##' @rdname as.data.table.xts
as.data.table <- function(x) {
    if (!requireNamespace("data.table", quietly=TRUE)) {
        stop("Cannot covert to 'data.table' without the 'data.table' package.", call.=FALSE)
    }
    UseMethod("as.data.table")
}

##' @rdname as.data.table.xts
as.data.table.default <- function(x, ...) {
    if (!requireNamespace("data.table", quietly=TRUE)) {
        stop("Cannot covert to 'data.table' without the 'data.table' package.", call.=FALSE)
    }
    as.data.table.default(x, ...)
}

## cf http://stackoverflow.com/a/27358897/143305
.gsee.as.data.table.xts <- function(x, ...) {
    cn <- colnames(x)
    sscn <- strsplit(cn, "\\.")
    xts::indexClass(x) <- c('POSIXct', 'POSIXt') #coerce index to POSIXct
    DT <- data.table::data.table(time=zoo::index(x), zoo::coredata(x))
    ##DT <- data.table(IDateTime(index(x)), coredata(x))

    ## If there is a Symbol embedded in the colnames, strip it out and make it a
    ## column
    if (all(sapply(sscn, "[", 1) == sscn[[1]][1])) {
        Symbol <- sscn[[1]][1]
        data.table::setnames(DT, names(DT)[-1], sub(paste0(Symbol, "."), "", cn))
        DT <- DT[, Symbol:=Symbol]
        data.table::setkey(DT, Symbol, time)[]
    } else {
        data.table::setkey(DT, time)[]
    }
}

utils::globalVariables(c(":="))
