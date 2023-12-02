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
    if (inherits(zoo::index(x), "Date")) {
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
    UseMethod("as.data.table", x)
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

##' Set threads for data.table respecting possible local settings
##'
##' This function set the number of threads \pkg{data.table} will use
##' while reflecting two possible machine-specific settings from the
##' environment variable \sQuote{OMP_THREAD_LIMIT} as well as the R
##' option \sQuote{Ncpus} (uses e.g. for parallel builds).
##' @title Set data.table threads respecting default settingss
##' @param ncores A numeric or character variable with the desired
##' count of threads to use
##' @param verbose A logical value with a default of \sQuote{FALSE} to
##' operate more verbosely
##' @return The return value of the \pkg{data.table} function
##' \code{setDTthreads} which is called as a side-effect.
##' @author Dirk Eddelbuettel
limitDataTableCores <- function(ncores, verbose = FALSE) {
    if (missing(ncores)) {
        ## start with a simple fallback: 'Ncpus' (if set) or else 2
        ncores <- getOption("Ncpus", 2L)
        ## also consider OMP_THREAD_LIMIT (cf Writing R Extensions), gets NA if envvar unset
        ompcores <- as.integer(Sys.getenv("OMP_THREAD_LIMIT"))
        ## and then keep the smaller
        ncores <- min(na.omit(c(ncores, ompcores)))
    }
    stopifnot("Package 'data.table' must be installed." = requireNamespace("data.table", quietly=TRUE))
    stopifnot("Argument 'ncores' must be numeric or character" = is.numeric(ncores) || is.character(ncores))
    if (verbose) message("Limiting data.table to '", ncores, "'.")
    data.table::setDTthreads(ncores)
}
