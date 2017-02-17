as.data.table.xts <- function(x) {
    dt <-  data.table(date=as.IDate(index(x)),
                      time=as.ITime(index(x)),
                      micros=round(1e6*(as.numeric(index(x)-trunc(index(x))))),
                      pt=as.numeric(index(x)),
                      coredata(x))
    setNumericRounding(0)
    setkey(dt, pt, date, time, micros)
    dt
}
