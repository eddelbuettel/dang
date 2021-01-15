##' List object and memory used by these objects
##'
##' These helper functions have evolved over the years; some were also
##' posted on StackOverflow in response to
##' \url{https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
##' @title List largest objects and show memory use
##' @param pos Position in search path, defaults to 1
##' @param pattern Pattern argument pass on to \code{ls}
##' @param order.by Optional sort order column
##' @param decreasing Optional switch for decreasing or increasing
##' sort order
##' @param head Optional switch to show \code{head(..., n)}
##' @param n Number of elements to show, default to 5
##' @return The displayed data.frame is returned to, the main purpose
##' effect however is the displayed information
##' @author Dirk Eddelbuettel, based on and extending code in mailing list
##' posts by Petr Pikal and David Hinds
ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    names <- ls(pos = pos, pattern = pattern)
    if (length(names) > 0) {      	# on startup, we have no symbols
        napply <- function(names, fn) sapply(names, function(x)
            fn(get(x, pos = pos)))
        obj.class <- napply(names, function(x) as.character(class(x))[1])
        obj.mode <- napply(names, mode)
        obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
        obj.size <- napply(names, object.size)
        obj.dim <- t(napply(names, function(x)
            as.numeric(dim(x))[1:2]))
        vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.dim)
        names(out) <- c("Type", "Size", "Rows", "Columns")
        if (!missing(order.by))
            out <- out[order(out[[order.by]], decreasing=decreasing), ]
        if (head)
            out <- head(out, n)
        out
    } else {
        #warning("No symbols...")
        NULL
    }
}

##' @rdname ls.objects
##' @param ... Passed through from \code{lsos} to \code{ls.objects}
lsos <- function(..., n=10) {
    ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

##' @rdname ls.objects
##' @param sort Sort columns, defauls to \sQuote{size}
##' @param limit Optional cap on displayed number of items
showMemoryUse <- function(sort="size", decreasing=FALSE, limit) {
    objectList <- ls(parent.frame())

    oneKB <- 1024
    oneMB <- 1048576
    oneGB <- 1073741824

    memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=x)))))

    memListing <- sapply(memoryUse, function(size) {
        if (size >= oneGB) return(paste(round(size/oneGB,2), "GB"))
        else if (size >= oneMB) return(paste(round(size/oneMB,2), "MB"))
        else if (size >= oneKB) return(paste(round(size/oneKB,2), "kB"))
        else return(paste(size, "bytes"))
    })

    memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)

    if (sort=="alphabetical")
        memListing <- memListing[order(memListing$objectName,decreasing=decreasing),]
    else
        ## will run if sort not specified or "size"
        memListing <- memListing[order(memoryUse,decreasing=decreasing),]

    if (!missing(limit)) memListing <- memListing[1:limit,]

    print(memListing, row.names=FALSE)
    return(invisible(memListing))
}
