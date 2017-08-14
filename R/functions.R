##' Debugging helper to assign formals from function
##'
##' (Beta) Attempt get default values from a given function,
##' extracting its arguments and assigning which should help in
##' debugging via \code{browser()} and other helper functions.
##' @title Assign formal arguments from function
##' @param f A function
##' @param env An environment to assign the values to, defaults
##' to global environment,
##' @return Nothing, but a side effect of assignment in global
##' environment
##' @author Dirk Eddelbuettel
assignFormals <- function(f, env=.GlobalEnv) {
    ff <- formals(f)
    for (n in names(ff)) {
        txt <- sprintf("%s <- %s", n, eval(ff[[n]]))
        assign(n, eval(ff[[n]]), envir=env)
    }
    invisible(NULL)
}
