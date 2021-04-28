fortif <- scan('fortif.txt')
#fortif <- ts(log(fortif), start=c(1949, 1), frequency=12)
fortif <- ts(log(fortif), start=c(1980, 1), frequency=12)

plot(fortif,ylab="thousands of litres",main="Australian sales of fortified wine")


logx <- window(fortif, end=c(1994, 12))
logx


plot(logx,ylab="log(thousands of litres)",main="Australian sales of fortified wine")

acf(logx)

d1logx <- diff(logx)
plot(d1logx)
acf(d1logx, lag=50)

d12d1logx <- diff(d1logx, lag=12)
plot(d12d1logx)
acf(d12d1logx, lag=50,main="acf")
pacf(d12d1logx, lag=50,main="pacf")


#decompose the time series 
decomp <- decompose(x, type="additive")
plot(decomp)

