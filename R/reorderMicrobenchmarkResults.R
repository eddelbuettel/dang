#' Reorder microbenchmark results
#'
#' The lovely `microbenchmark` package returns its result is a simple structure
#' with `factor` variable which it leaves unordered.  The printed as well as
#' plotted results become a little more expressive if an order is added, which
#' this helper function does.
#'
#' 'rmr' can be used as a shorter alias
#'
#' @title Reorder microbenchmark Results
#' @param res An object returned from `microbenchmark::microbenchmark`
#' @param order An option character variable selecting a column to order on,
#' defaults to 'median'
#' @return A modified version of `res` with an ordering by `order`
reorderMicrobenchmarkResults <- function(res, order="median") {
    stopifnot("Argument 'res' must be a 'microbenchmark' result" = inherits(res, "microbenchmark"))

    smry <- summary(res)
    res$expr <- factor(res$expr,
                       levels = levels(res$expr)[order(smry[["median"]])],
                       ordered = TRUE)
    res
}

#' @rdname reorderMicrobenchmarkResults
rmr <- reorderMicrobenchmarkResults
