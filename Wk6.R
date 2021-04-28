x <- scan('airmiles.txt', skip=3) #read in the data 

x <- ts(x, start=c(1960, 1), frequency=12) #convert into time series 

par(mfrow=c(1,2)) 

plot(x) 

logx <- log(x) #take log 

plot(logx) 



acf(logx) 

d1logx <- diff(logx) 

plot(d1logx) 



d12d1logx <- diff(d1logx, lag=12) 

acf(d12d1logx, lag.max=50) 

pacf(d12d1logx, lag.max=50) 