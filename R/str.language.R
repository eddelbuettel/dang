## source:
##   From: Bill Dunlap <williamwdunlap@gmail.com>
##   To: Duncan Murdoch <murdoch.duncan@gmail.com>
##   Cc: r-devel <r-devel@r-project.org>
##   Subject: Re: [Rd] Parser oddity with <- and =
##   Date: Fri, 4 Feb 2022 12:28:12 -0800

#' Print a parse tree
#'
#' @param expr An R language expression to be parse and displayed
#' @param name An optional character value with default '' to annotate
#' the display
#' @param indent An optional numeric value with default 0 to provide
#' additional indentation
#' @return The expression, invisibly
#' @author Bill Dunlap (and posted to r-devel on 4 Feb 2022)
str.language <- function(expr, name = "", indent = 0) {
    trim... <- function(string, width.cutoff) {
        if (nchar(string) > width.cutoff) {
            string <- sprintf("%.*s ...", width.cutoff-4, string)
        }
        string
    }
    cat(sep="", rep("  ", indent), typeof(expr), ": ",
        if (length(name)==1 && nzchar(name)) { paste0(name, " = ") },
        trim...(deparse1(expr, width.cutoff=40), width.cutoff=40),
        "\n")
    if (is.function(expr)) {
        str.language(formals(expr), name="[formals]", indent = indent + 1)
        str.language(body(expr), name="[body]", indent = indent + 1)
    } else if (is.recursive(expr)) {
        expr <- as.list(expr)
        nms <- names(expr)
        for (i in seq_along(expr)) {
            str.language(expr[[i]], name=nms[[i]], indent = indent + 1)
        }
    }
    invisible(expr)
}
