##' Fetch a real-time market data series from AlphaVantage
##'
##' Several optional parameters could be set, but are not currently.
##' @title Retrieve real-time data from AlphaVantage
##' @param sym Character value for the ticker
##' @return A data.table object
##' @author Dirk Eddelbuettel
alphavantage <- function(sym) {
    cmd <- paste0("https://www.alphavantage.co/query?",
                  "function=TIME_SERIES_INTRADAY&",
                  "symbol=", sym,
                  "&interval=1min&",
                  "apikey=", getOption("alphavantageKey", "demo"), "&",
                  "datatype=csv&",
                  "outputsize=compact")
    if (!requireNamespace("data.table", quietly=TRUE)) {
        stop("The 'data.table' package needs to be installed.", call. = FALSE)
    }
    data <- data.table::fread(cmd, showProgress=FALSE)
}
