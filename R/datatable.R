as.data.table.xts <- function(x) {
    dt <-  data.table(date=as.IDate(index(x)),
                      time=as.ITime(index(x)),
                      micros=round(1e6*(as.numeric(index(x)-trunc(index(x))))),
                      coredata(x))
    setkey(dt, date, time, micros)
    dt
}
