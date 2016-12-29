print.xts <- function(x, n=5) {
    if (nrow(x) <= 6) {
        print(as.data.frame(x))
    } else {
        print(head(as.data.frame(x),n))
        cat("...\n")
        print(tail(as.data.frame(x),n))
    }
}
