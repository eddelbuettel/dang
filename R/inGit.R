
##' The \code{getGitRoot()} function recursively ascends the
##' filesystem tree from the given directory until it either finds a
##' directory \code{.git}, or the top-level directory to abort the
##' search. The root directory of the \code{git} repository is
##' returned, with an exmpty string in the case of no repository.  The
##' \code{inGit()} function turns this into boolean predicate returning
##' either \code{TRUE} or \code{FALSE}.
##'
##' @title Are we in a \code{git} repository?
##' @param cwd The start directory, default to the current working
##' directory
##'
##' @return For \code{getGitRoot()}, the path of the directory containing
##'  the \code{.git} directory, ie the project root directory, or an
##'  empty string in case the search started outside a \code{git} directory.
##'  The \code{inGit()} function returns a boolean as to whether a \code{git}
##'  repository was found or not.
##' @author Dirk Eddelbuettel
##' @examples
##' inGit()
getGitRoot <- function(cwd=getwd()) {
    isTop <- function(cwd) normalizePath(cwd) == normalizePath(file.path(cwd, ".."))
    if (dir.exists(file.path(cwd, ".git"))) return(cwd)
    parent <- normalizePath(file.path(cwd, ".."))
    if (parent == "/" || isTop(parent)) return("")
    return(getGitRoot(parent))
}

##' @rdname getGitRoot
inGit <- function(cwd=getwd()) {
    getGitRoot(cwd) != ""
}
