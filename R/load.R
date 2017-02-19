##' Silently attach a library to the search path.
##'
##' This function wraps \code{suppressMessages} around the call to
##' \code{library}
##' @title Silently load a library
##' @param ... Passed though
##' @return Nothing, but the desired library is attached
##' @author Dirk Eddelbuettel
silent <- function(...) suppressMessages(library(...))
