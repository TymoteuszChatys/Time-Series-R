
Box.test(x,k,type="Ljung-Box") #gives a p-value for this test using data in x
#The test is significant at level alpha if p-value < alpha. Otherwise the test is not significant

x <- rnorm(100) #r for random, norm for normal
plot(x, type="l")

#par(mfrow=c(2,1)) #split graph window into 2
#acf(x,20,plot=F) #sample acf at lags up to 20
#pacf(x,20,plot=F) #sample pacf at lags up to 20

Box.test(x,5,type="Ljung-Box")
