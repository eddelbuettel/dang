
##' Display a demotivating quote to remind the users of the harsh reality
##' econometric and statistical practice.
##'
##' This function is a port of \code{demotivate} command for 'Stata' from the
##' ado file at \url{http://fmwww.bc.edu/repec/bocode/d/demotivate.ado}.
##' @title Display a demotivating quote
##' @param x A index value chosen to select a quote; if not given a quote is
##' chosen randomly.
##' @param width The desired display width.
##' @return The formatted is returned invisibly.
##' @seealso \url{https://ideas.repec.org/c/boc/bocode/s458576.html}
##' @author Dirk Eddelbuettel for this function, and Kevin Denny for the
##' 'Stata' original.
##' @examples demotivate()
demotivate <- function(x, width = NULL) {
    txt <- c("'All models are wrong but some are useful.' George Box. Indeed but its unclear that you can take much consolation from that.",
             "Those last set of results you got: not exactly AER material are they?",
             "Have you thought of trying economic theory? It worked out well for Paul Krugman.",
             "Seriously, nobody believes your p values.",
             "Reviewer #2 is going to have fun if you include your latest results.",
             "You need to stop thinking like a graduate student. Especially if you are one.",
             "John Ioannidis has got you sussed out. All of us in fact. We're doomed I tell you.",
             #"Best not to dwell on what your R-using colleagues really think of you.",
             "'Friends don't let friends use instrumental variables.' Unknown.",
             "This will be a lot easier to do in the next version of Stata. Kerching $$$$$ !",
             "The fact is most papers are rejected.",
             #"It's not too late to learn Python",
             "If only you had taken a few more math courses you could have been someone. You could have been a contender.",
             "Life is full of emptiness.",
             "'Imagine how hard physics would be if electrons had feelings.' Richard Feynmann. Welcome to our world, Dick.",
             "'There are no routine statistical questions only questionable statistical routines.' David Cox. Here's looking at you, kid.",
             "'But our preferred model shows...'  lmao, gets me everytime.",
             "'Erode gormless sin' is an anagram for 'regression models' curiously enough.",
             "Don't worry: there is a behavioral economics interpretation of your results.",
             "Have you ever considered letting Bayes into your life?" )

    if (missing(x) || x < 1 || x > length(txt)) x <- sample(length(txt), 1)
    if (is.null(width)) width <- 0.9 * getOption("width")
    if (width < 10) stop("'width' must be greater than 10", call.=FALSE)
    invisible(sapply(strwrap(txt[x], width), cat, "\n"))
}
