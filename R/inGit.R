##' This function recursively ascends the filesystem tree from the
##' given directory until it either finds a directory \code{.git}, or
##' the top-level directory to abort the search.
##'
##' On Windows, \code{FALSE} is returned unconditionally.
##' @title Are we in a \code{git} repository?
##' @param cwd The start directory, default to the current working
##' directory
##' @return The path of the directory containing the \code{.git}
##'  directory, ie the project root directory, or an empty string
##'  in case the search started outside a \code{git} directory.
##' @author Dirk Eddelbuettel
##' @examples
##' inGit()
inGit <- function(cwd=getwd()) {
    #print(cwd)
    #print(".." %in% dir(cwd, all.files=TRUE, include.dirs=TRUE))
    if (.Platform$OS.type == "windows") return(FALSE)
    if (dir.exists(file.path(cwd, ".git"))) return(cwd)
    parent <- normalizePath(file.path(cwd, ".."))
    if (parent == "/") return("")
    return(inGit(parent))
}
