
## cf https://gist.github.com/joshuaulrich/ee11ef67b1461df399b84efd3c8f9f67#file-intraday-sp500-r

##' Intra-day Market Monitor for Security Prices
##'
##' This function periodically queries a public data source for a current price of given
##' symbol and updates an intra-daily chart for that security. A working example is symbol
##' \dQuote{^GSPC} for the S&P500 index which can be obtained in real-time during (New York)
##' trading hours. Other symbols may work.
##'
##' An alternate version offering 24-hour coverage, for example for futures on Globex, is
##' being prepared.
##'
##' The function could be further generalized in numerous way and should be considered
##' \sQuote{alpha}. Current default values are a 15 second sleep, and fixed cut-off times
##' for market open/close states corresponding to NYSE hours. The data history is reset to
##' the two most recent days at the close, amd the data is snapshot to file (with the
##' filename derived from the symbol, and the path given by \code{tools::R_user_dir}).
##' These parameters might become configuration parameters in the future.
##'
##' @title Intra-day market monitor
##' @param symbol A character variable with symbol understood by \code{getQuote} from
##' package \pkg{quantmod}, default value is \dQuote{^GSPC}.
##' @param defaultTZ A character variable with the (local) timezone used for displaying
##' the data, default value is \dQuote{America/Chicagp}.
##' @return Nothing is returned, but a display of the current price and the recent history
##' is updated, and the loops loops \sQuote{forever}.
##' @seealso \url{https://gist.github.com/joshuaulrich/ee11ef67b1461df399b84efd3c8f9f67#file-intraday-sp500-r}
##' @author Dirk Eddelbuettel extending and refactoring the original code by Josh Ulrich
##' @examples
##' if (requireNamespace("quantmod", quietly)) {           # only suggested packages used
##'    suppressMessages({library(xts);library(quantmod)})  # dampen noise, add dang as needed
##'    intradayMarketMonitor()
##' }
intradayMarketMonitor <- function(symbol = "^GSPC", defaultTZ = "America/Chicago") {
    stopifnot(`The quantmod packages is required.`=requireNamespace("quantmod", quietly=TRUE))

    x <- NULL
    datafile <- .default_file(symbol)
    if (file.exists(datafile)) {
        x <- .most_recent_n_days(readRDS(datafile))
        .show_plot(symbol, x)
    }

    market_closed <- TRUE
    errored <- FALSE
    repeat {
        curr_t <- Sys.time()
        now <- .hourmin(curr_t)
        if (now >= 1500) {
            ## we need an NA observations to plot a gap
            if (!market_closed) {
                y <- xts::xts(data.frame(Open=NA,High=NA,Low=NA,Close=NA,Volume=0), trunc(curr_t))
                x <- rbind(x, y)
                saveRDS(x, datafile)
                .msg(curr_t, "after close; set NA + wrote data, sleeping")
            } else {
                .msg(curr_t, "after close; sleeping")
            }
            x <- .most_recent_n_days(x)      # subset data
            market_closed <- TRUE
            tgt <- as.POSIXct(paste(format(as.Date(curr_t)), "23:59:59.999"))
            dt <- ceiling(as.numeric(difftime(tgt, curr_t, units="mins")))
            Sys.sleep(dt*60)
            next
        } else if (now < 830) {
            market_closed <- TRUE
            tgt <- as.POSIXct(paste(format(as.Date(curr_t)), "08:29:59.999"))
            dt <- max(1L, round(as.numeric(difftime(tgt, curr_t, units="secs"))))
            .msg(curr_t, "before open; sleeping for", dt, "secs or", round(dt/60,0), "mins")
            x <- .most_recent_n_days(x)      # subset data
            Sys.sleep(dt)
            next
        } else if (now >= 830 && market_closed) {
            .msg(curr_t, "market open")
            market_closed <- FALSE
        }
        y <- try(.get_data(symbol, defaultTZ), silent = TRUE)
        if (inherits(y, "try-error")) {
            .msg(curr_t, "Error:", attr(y, "condition")[["message"]])
            errored <- TRUE
            Sys.sleep(10)
            next
        } else if (errored) {
            errored <- FALSE
            .msg(curr_t, "...recovered")
        }
        if (!market_closed) {
            dataDate <- as.Date(zoo::index(tail(y,1)))
            currDate <- as.Date(curr_t)
            if (currDate == dataDate) {
                x <- rbind(x, y)
            } else {
                .msg(curr_t, "Skip previous close", format(zoo::index(y)),
                     "close", y[1,"Close"], "pct_change", attr(y,"pct_change"))
            }
        }
        if (nrow(x) >= 4) .show_plot(symbol, x, y)
        Sys.sleep(10)
    }
    # may not get here if Ctrl-C aborted
    saveRDS(x, datafile)
}

## unexported helper functions below

.default_file <- function(symbol) {
    nm <- paste0("intraday_", make.names(symbol), ".rds")
    dd <- tools::R_user_dir("dang")
    if (!dir.exists(dd)) dir.create(dd)
    fname <- file.path(dd, nm)
    fname
}

.most_recent_monday <- function() {
    d <- Sys.Date()
    while (as.POSIXlt(d)$wday != 1) d <- d - 1
    d
}

.most_recent_n_days <- function(x, n=2, minobs=1000) {
    tt <- table(as.Date(zoo::index(x)))
    cutoff <- as.Date(names(head(tail(tt[tt>minobs], n), 1)))
    x[ as.Date(zoo::index(x)) >= cutoff ]
}

.show_plot <- function(symbol, x, y) {
    cname <- paste(symbol, format(quantmod::Cl(xts::last(na.omit(x)))), sep="\t")
    if (!missing(y)) cname <- paste(cname, round(attr(y, "pct_change"), 5), sep = "\t")
    cs <- quantmod::chart_Series(quantmod::Cl(x), name = cname)
    plot(cs)
}

.get_data <- function(symbol, tz) {
    quote <- quantmod::getQuote(symbol)
    attr(quote$`Trade Time`, "tzone") <- tz
    quote$Close <- quote$Last
    xts::xts(quantmod::OHLCV(quote), quote[,"Trade Time"], pct_change = quote[,"% Change"])
}

.msg <- function(ts, ...) {
    op <- options(digits.secs=3)
    cat(format(ts), ..., "\n")
    options(op)
}

.hourmin <- function(ts) {
    now_t <- xts::xts(, ts)
    xts::.indexhour(now_t)*100 + xts::.indexmin(now_t)
}
