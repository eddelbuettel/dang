
## plotOBOS -- displaying overbough/oversold as eg in Bespoke's plots
##
## Copyright (C) 2010 - 2023  Dirk Eddelbuettel
##
## This is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.

##' Compute and display overbought and oversold regions
##'
##' This function computes a smoothed version of the price using a
##' moving average (with one of several possible methods) as well as a
##' standard deviation band, and displays one and two standard
##' deviations around the smoothed price.
##' @title Plot Overbought/Oversold Regions for A Stock or ETF
##' @param symbol A (required) character string for stock symbol, or
##' alternatively a \code{xts} or \code{zoo} object with data
##' @param n An optional integer for the moving average length,
##' defaults to 50
##' @param type An optional character string for the type of moving
##' average; currently supported are \sQuote{SMA}, \sQuote{EMA},
##' \sQuote{ZLEMA}, and \sQuote{HMA}.
##' @param years An optional numeric or integer value for the number
##' of years of data to display, defaults to one
##' @param blue An optional boolean determining whether blue or gray
##' tones are used, defaults to true implying blue tones
##' @param current An optional boolean determining whether the current
##' date is the end date
##' @param title An optional character string for the plot title, defaults
##' to the symbol
##' @param ticks An optional boolean indicating whether ticks are plotted,
##' passed on to \code{plot.xts}, defaults to true
##' @param axes An optional boolean indicating whether axes are plotted,
##' passed on to \code{plot.xts}, defaults to true
##' @return \code{NULL} as the function is invoked for the side effect of
##' the plot
##' @author Dirk Eddelbuettel
plotOBOS <- function(symbol, n=50, type=c("sma", "ema", "zlema", "hma"),
                     years=1, blue=TRUE, current=TRUE, title=symbol,
                     ticks=TRUE, axes=TRUE) {

    today <- Sys.Date()
    if (is.character(symbol)) {
        X <- quantmod::getSymbols(symbol, from=format(today-365*years-2*n), auto.assign=FALSE)
        x <- X[,6]                          # use Adjusted
    } else if (inherits(symbol, "zoo")) {
        x <- X <- xts::as.xts(symbol)
        current <- FALSE                # don't expand the supplied data
    }

    n <- min(nrow(x)/3, 50)             # as we may not have 50 days

    sub <- ""
    if (current) {
        xx <- quantmod::getQuote(symbol)
        xt <- xts::xts(xx$Last, order.by=as.Date(xx$`Trade Time`))
        colnames(xt) <- paste(symbol, "Adjusted", sep=".")
        x <- rbind(x, xt)
        sub <- paste("Last price: ", xx$Last, " at ",
                     format(as.POSIXct(xx$`Trade Time`), "%H:%M"), sep="")
    }

    type <- match.arg(type)
    xd <- switch(type,                  # compute xd as the central location via selected MA smoother
                 sma = TTR::SMA(x,n),
                 ema = TTR::EMA(x,n),
                 zlema = TTR::ZLEMA(x,n),
                 hma = TTR::HMA(x,n))
    xv <- TTR::runSD(x, n)              # compute xv as the rolling volatility

    strt <- paste(format(today-365*years), "::", sep="")
    x  <- x[strt]                       # subset plotting range using xts' nice functionality
    xd <- xd[strt]
    xv <- xv[strt]

    xyd <- xy.coords(xts::.index(xd),xd[,1]) # xy coordinates for direct plot commands
    xyv <- xy.coords(xts::.index(xv),xv[,1])

    n <- length(xyd$x)
    xx <- xyd$x[c(1,1:n,n:1)]           # for polygon(): from first point to last and back

    if (blue) {
        blues5 <- c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C") # cf brewer.pal(5, "Blues")
        fairlylight <- rgb(189/255, 215/255, 231/255, alpha=0.625) # aka blues5[2]
        verylight <- rgb(239/255, 243/255, 255/255, alpha=0.625) # aka blues5[1]
        dark <- rgb(8/255, 81/255, 156/255, alpha=0.625) # aka blues5[5]
    } else {
        fairlylight <- rgb(204/255, 204/255, 204/255, alpha=0.5)  # two suitable grays, alpha-blending at 50%
        verylight <- rgb(242/255, 242/255, 242/255, alpha=0.5)
        dark <- 'black'
    }

    plot(x, ylim=range(range(x, xd+2*xv, xd-2*xv, na.rm=TRUE)), main=title, sub=sub,
         major.ticks=ticks, minor.ticks=ticks, axes=axes) # basic xts plot setup
    xts::addPolygon(xts::xts(cbind(xyd$y+xyv$y, xyd$y+2*xyv$y), order.by=zoo::index(x)), on=1, col=fairlylight)  # upper
    xts::addPolygon(xts::xts(cbind(xyd$y-xyv$y, xyd$y+1*xyv$y), order.by=zoo::index(x)), on=1, col=verylight)    # center
    xts::addPolygon(xts::xts(cbind(xyd$y-xyv$y, xyd$y-2*xyv$y), order.by=zoo::index(x)), on=1, col=fairlylight)  # lower
    lines(xd, lwd=2, col=fairlylight)   # central smooted location
    lines(x, lwd=3, col=dark)           # actual price, thicker
}
