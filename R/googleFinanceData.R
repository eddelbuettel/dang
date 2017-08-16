
##' Download historical time series from Google Finance
##'
##' The function uses an (unofficial) older CGI-style interface at
##' Google to download historical data.
##'
##' @title Historical Price Data Download from Google Finance
##' @param sym A character string for a (tradeable) symbol
##' @param current A logical switch to indicate whether the current time is the endtime
##' @param sy An integer value for the start year, default is 2005
##' @param sm An integer value for the start month, default is 1
##' @param sd An integer value for the start date, default is 1
##' @param ey An optional integer value for the end year, required if \code{current} is false
##' @param em An optional integer value for the end month, required if \code{current} is false
##' @param ed An optional integer value for the end day, required if \code{current} is false
##' @return A \code{data.table} object with a key on \code{date}
##' @seealso The post at
##' \url{https://chrisconlan.com/download-historical-stock-data-google-r-python/}
##' provided the initial starting point
##' @author Dirk Eddelbuettel
googleFinanceData <- function(sym, current = TRUE, sy = 2005, sm = 1, sd = 1, ey, em, ed) {
    ## sy, sm, sd, ey, em, ed correspond to
    ## start year, start month, start day, end year, end month, and end day

    ## If TRUE, use the date as the enddate
    if (current){
        systime <- as.character(Sys.time())
        ey <- as.numeric(substr(systime, start = 1, stop = 4))
        em <- as.numeric(substr(systime, start = 6, stop = 7))
        ed <- as.numeric(substr(systime, start = 9, stop = 10))
    }

    ## Fetch data from google
    url <- paste0("http://www.google.com/finance/historical",
                  "?q=", sym,
                  "&startdate=", paste(sm, sd, sy, sep = "+"),
                  "&enddate=", paste(em, ed, ey, sep = "+"),
                  "&output=csv")
    tfile <- tempfile(fileext=".csv")
    download.file(url, destfile=tfile, quiet=TRUE)
    data <- data.table::fread(tfile)

    ## If successful, rename first column
    if (!is.null(data)) {
        names(data)[1] <- "Date"
        data[[1]] <- as.Date(data[[1]], "%d-%b-%y")

        data.table::setkey(data, "Date")
    }

    return(data)
}










