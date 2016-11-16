print.xts <- function(x, n=5) {
    print(head(as.data.frame(x),n))
    cat("...\n")
    print(tail(as.data.frame(x),n))
}
