
##' Display a motivating quote for users.
##'
##' This function is a port of \code{motivate} command for 'Stata' from the
##' ado file at \url{http://fmwww.bc.edu/repec/bocode/m/motivate.ado}
##' @title Display a motivating quote
##' @param x A index value chosen to select a quote; if not given a quote is
##' chosen randomly.
##' @param width The desired display width.
##' @return The formatted is returned invisibly.
##' @seealso \url{https://ideas.repec.org/c/boc/bocode/s458565.html}
##' @author Dirk Eddelbuettel for this function, and Kabira Namit for the
##' 'Stata' original.
##' @examples motivate()
motivate <- function(x, width = NULL) {
    txt <- c("'I find that the harder I work, the more luck I seem to have.'  Thomas Jefferson",
             "'Success is not final, failure is not fatal. It is the courage to continue that counts.'  Winston Churchill",
             "'If you are going through hell, keep going.'  Winston Churchill",
             #"'Patience and deep breaths are key, but so is Statalist.' Sara Ansari",
             #"'Maybe Nick Cox has written something about this error?' Kabira Namit",
             "'Nothing in the world is worth having or worth doing unless it means effort, pain, difficulty. I have never in my life envied a human being who led an easy life. I have envied a great many people who led difficult lives and led them well.'  Theodore Roosevelt",
             #"'I did not mess up the syntax. I just found 100 ways to specify it incorrectly.' Kabira Namit"
             "'It always seems impossible till it is done.'  Nelson Mandela",
             "'If you were able to believe in Santa Claus for 8 years, you can believe in yourself for 5 minutes.'  Unknown",
             "'I have failed over and over and over again in my life. And that is why I succeed.'  Michael Jordan",
             #"'Every Stata expert has had their fair share of errors. Keep at it!'  Kabira Namit",
             #"'Does ', force' still work? Asking for a friend.'  Paul Atherton",
             "'There are two kinds of people in this world: those who want to get things done and those who do not want to make mistakes.'  John Maxwell",
             "'If we knew what we were doing, it would not be called research, would it?'  Albert Einstein",
             "'We must accept finite disappointment but never lose infinite hope.'  Martin Luther King",
             "'The only real mistake is the one from which we learn nothing.'  Henry Ford",
             "'Try to live everyday like Elle Woods after Warner told her she was not smart enough for law school.'  Marissa Kathryn",
             "'Failure is so important. We speak about success all the time. It is the ability to resist failure or use failure that often leads to greater success. I have met people who do not want to try for fear of failing.'  Joanne K. Rowling",
             "'A person who never made a mistake never tried anything new.'  Albert Einstein",
             "'Success consists of going from failure to failure without loss of enthusiasm.'  Winston Churchill",
             "'There is beauty in every struggle that is your own.'  Salvia Zeeshan")

    if (missing(x) || x < 1 || x > length(txt)) x <- sample(length(txt), 1)
    if (is.null(width)) width <- 0.9 * getOption("width")
    if (width < 10) stop("'width' must be greater than 10", call.=FALSE)
    invisible(sapply(strwrap(txt[x], width), cat, "\n"))
}
