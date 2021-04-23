ARMAacf(ar=0.3,ma=0.4, lag.max=5)
ARMAacf(0.3,0.4,5,pacf=T)

ARMAacf(ar=c(0.3,0.4), lag.max=5)
ARMAacf(ar=c(0.3,0.4), lag.max=5, pacf=T)

ARMAacf(ar=c(-0.3,-0.4), lag.max=5)
ARMAacf(ar=c(-0.3,-0.4), lag.max=5, pacf=T)

ARMAacf(ar=c(0.4,rep(0,10),0.5,-0.2), lag.max=24)
ARMAacf(ar=c(0.4,rep(0,10),0.5,-0.2), lag.max=24, pacf=T)



data <- read.table("d_logret_6stocks.txt", header=T)
library(FinTS)

par(mfrow=c(3,2))
for (i in 1:6) {
  Acf(data[,i+1], lag.max=25, main=names(data)[i+1])
  print(Box.test(data[, i+1], lag=20, type="Ljung-Box"))
}


