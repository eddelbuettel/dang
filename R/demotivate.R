
##' Display a demotivating quote to remind the users of the harsh reality
##' econometric and statistical practice.
##'
##' This function is a port of \code{demotivate} command for 'Stata' from the
##' ado file at \url{http://fmwww.bc.edu/repec/bocode/d/demotivate.ado}.
##' @title Display a demotivating quote
##' @param x A value between 0 and 1 chosen to select a quote; if not given a random
##' uniform draw is made.
##' @param width The desired display width.
##' @return The formatted is returned invisibly.
##' @seealso \url{https://ideas.repec.org/c/boc/bocode/s458576.html}
##' @author Dirk Eddelbuettel for this function, and Kevin Denny for the 'Stata' original.
##' @examples demotivate()
demotivate <- function(x, width = NULL) {
    if (missing(x)) x <- runif(1)

    if (x <= 0.05) {
        txt <- paste0("\"All models are wrong but some are useful.\" George Box.",
                      "Indeed but its unclear that you can take much consolation from that.")
    } else if (x <= 0.10) {
        txt <- "Those last set of results you got: not exactly AER material are they?"
    } else if (x <= 0.15) {
        txt <- "Have you thought of trying economic theory? It worked out well for Paul Krugman."
    } else if (x <= 0.20) {
        txt <- "Seriously, nobody believes your p values."
    } else if (x <= 0.25) {
        txt <- "Reviewer #2 is going to have fun if you include your latest results."
    } else if (x <= 0.30) {
        txt <- paste0("You need to stop thinking like a graduate student. Especially if you are one.",
                      "                                            ")
    } else if (x <= 0.35) {
        txt <- paste0("John Ioannidis has got you sussed out. All of us in fact. ",
                      "We're doomed I tell you.")
    } else if (x <= 0.40) {
        txt <- "Best not to dwell on what your R-using colleagues really think of you."
    } else if (x <= 0.45) {
        txt <- "\"Friends don't let friends use instrumental variables.\" Unknown."
    } else if (x <= 0.50) {
        txt <- "This will be a lot easier to do in the next version of Stata. Kerching $$$$$ !"
    } else if (x <= 0.55) {
        txt <- "The fact is most papers are rejected"
    } else if (x <= 0.60) {
        txt <- "It's not too late to learn Python"
    } else if (x <= 0.65) {
        txt <- paste0("If only you had taken a few more math courses you could have been ",
                      "someone. You could have been a contender.")
    } else if (x <= 0.70) {
        txt <- paste0("Life is full of emptiness")
    } else if (x <= 0.75) {
        txt <- paste0("\"Imagine how hard physics would be if electrons had feelings.\" ",
                      "Richard Feynmann. Welcome to our world, Dick.")
    } else if (x <= 0.80) {
        txt <- paste0("\"There are no routine statistical questions only questionable ",
                      "statistical routines.\" David Cox. Here's looking at you, kid.")
    } else if (x <= 0.85) {
        txt <- "\"But our preferred model shows...\"  lmao, gets me everytime"
    } else if (x <= 0.90) {
        txt <- "\"Erode gormless sin\" is an anagram for \"regression models\" curiously enough"
    } else if (x <= 0.95) {
        txt <- "Don't worry: there is a behavioral economics interpretation of your results."
    } else { #if (x <= 1.00) {
        txt <- "Have you ever considered letting Bayes into your life?"
    }

    if (is.null(width)) width <- 0.9 * getOption("width")
    if (width < 10) stop("'width' must be greater than 10", call.=FALSE)
    invisible(sapply(strwrap(txt, width), cat, "\n"))
}
