# TSA week 10

# Use different models/methods for prediction and compare results

# Example 1. Prediction of log airline passenger numbers 

#read in the data, take log and make time series object
airpass <- scan('airpass.txt')
airpass <- ts(log(airpass), start=c(1949, 1), frequency=12)     
airpass

#leave out year 1960 (to be predicted)                  
x <- window(airpass, end=c(1959, 12))
x

#decompose the time series 
decomp <- decompose(x, type="additive")
plot(decomp)

#look at different parts 
decomp$trend
decomp$seasonal
decomp$random

#remove missing values from the error part 
x <- decomp$random
x <- window(x, start=c(1949,7), end=c(1959,6)) 	

#fit the same ARIMA models as before
fit2 <- arima(x, order=c(0,0,5), seasonal=list(order=c(1,0,0), period=12), include.mean=F)
tsdiag(fit2, lag=50)

fit5 <- arima(x, order=c(2,0,3), seasonal=list(order=c(1,0,0), period=12), include.mean=F)
tsdiag(fit5, lag=50)

fit6 <- arima(x, order=c(5,0,0), seasonal=list(order=c(1,0,0), period=12), include.mean=F)
tsdiag(fit6, gof.lag=50)

#rename them 
fit05 <- fit2 
fit23 <- fit5
fit50 <- fit6

#plot the error part and leave room for predictions 
par(mfrow=c(1,1))
plot(window(x, end=c(1960, 12), extend=T)) 

#make predictions and add to plot 
pred05 <- predict(fit05, n.ahead=6+12)$pred
pred23 <- predict(fit23, n.ahead=6+12)$pred
pred50 <- predict(fit50, n.ahead=6+12)$pred

lines(pred05, col="red")
lines(pred23, col="green")
lines(pred50, col="blue")

#duplicate seasonal part of 1959 to use for 1960 
cycle <- window(decomp$seasonal, start=c(1959,1))
cycle
cycle <- ts(cycle, start=c(1960,1), frequency=12)
cycle

#look at the trend part 
y <- decomp$trend
y

#remove missing values
y <- window(y, start=c(1949,7), end=c(1959,6))

#month numbers till June 1959
t <- 7:(length(y)+6)
t

#fit a linear function to the trend
fit <- lm(y~t)		 

#predict for 1960 
pred <- predict(fit, newdata=list(t=max(t)+(1:18)))
pred <- ts(pred, start=c(1959,7), frequency=12)
pred

plot(airpass)
lines(pred+cycle+pred05, col="red")
lines(pred+cycle+pred23, col="green")
lines(pred+cycle+pred50, col="blue")

err05 <- airpass-pred-cycle-pred05
err05
err23 <- airpass-pred-cycle-pred23
err50 <- airpass-pred-cycle-pred50

mape05 <- mean(abs(err05/airpass))*100
mape23 <- mean(abs(err23/airpass))*100
mape50 <- mean(abs(err50/airpass))*100
c(mape05, mape23, mape50)

mad05 <- mean(abs(err05))
mad23 <- mean(abs(err23))
mad50 <- mean(abs(err50))
c(mad05, mad23, mad50)

msd05 <- mean(err05^2)
msd23 <- mean(err23^2)
msd50 <- mean(err50^2)
c(msd05, msd23, msd50)

# arima(0,1,1)x(0,1,1)_12

x <- window(airpass, end=c(1959,12))

fit011 <- arima(x, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
pred011 <- predict(fit011, n.ahead=12)$pred

err011 <- airpass - pred011
err011

mape011 <- mean(abs(err011/airpass))*100
mad011 <- mean(abs(err011))
msd011 <- mean(err011^2)

c(mape05, mape23, mape50, mape011)
c(mad05, mad23, mad50, mad011)
c(msd05, msd23, msd50, msd011)

# triple exponential smoothing

x <- window(airpass, end=c(1959,12))
es_add <- HoltWinters(x, seasonal="additive")
pred_es <- predict(es_add, n.ahead=12)

err_es <- airpass - pred_es
err_es

plot(airpass)
lines(pred_es, col="red")

mape_es <- mean(abs(err_es/airpass))*100
mad_es <- mean(abs(err_es))
msd_es <- mean(err_es^2)

c(mape05, mape23, mape50, mape011, mape_es)
c(mad05, mad23, mad50, mad011, mad_es)
c(msd05, msd23, msd50, msd011, msd_es)

#regression with correlated errors

x <- window(airpass, end=c(1959,12))

#time 
t <- 1 : length(x)
t

#months 
m <- rep(c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", 
           "Sept", "Oct", "Nov", "Dec"), 11)
m
m <- factor(m)
m

#estimate trend and seasonal component using least squares
#not the best method, but need the residuals
fit <- lm(x ~ t+m)
fit

resid <- resid(fit)
resid <- ts(resid, start=c(1949,1), frequency=12)
plot(resid)

#identify correlation structure from residuals 
par(mfrow=c(2,1))
acf(resid, lag=50); pacf(resid, lag=50)

#dummy variables 
m1 <- (m == "Jan")
m2 <- (m == "Feb")
m3 <- (m == "Mar")
m4 <- (m == "Apr")
m5 <- (m == "May")
m6 <- (m == "June")
m7 <- (m == "July")
m8 <- (m == "Aug")
m9 <- (m == "Sept")
m10 <- (m == "Oct")
m11 <- (m == "Nov")
m12 <- (m == "Dec")

#'x' for exogenous, from outside the time series 
#'reg' for regressors 
xreg <-cbind(t, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12) 

#estimate regression and AR(1) parameter at the same time 
fit_ar1 <- arima(x, order=c(1,0,0), xreg=xreg)
fit_ar1

tsdiags(fit_ar1, 50)

#xreg for 1960 
newxreg <- rbind(c(133, 0,0,0,0,0,0,0,0,0,0,0), #Jan 1960
                 c(134, 1,0,0,0,0,0,0,0,0,0,0), #Feb 1960
                 c(135, 0,1,0,0,0,0,0,0,0,0,0), #Mar 1960
                 c(136, 0,0,1,0,0,0,0,0,0,0,0), #Apr 1960
                 c(137, 0,0,0,1,0,0,0,0,0,0,0), #May 1960
                 c(138, 0,0,0,0,1,0,0,0,0,0,0), #June 1960
                 c(139, 0,0,0,0,0,1,0,0,0,0,0), #July 1960
                 c(140, 0,0,0,0,0,0,1,0,0,0,0), #Aug 1960
                 c(141, 0,0,0,0,0,0,0,1,0,0,0), #Sept 1960
                 c(142, 0,0,0,0,0,0,0,0,1,0,0), #Oct 1960
                 c(143, 0,0,0,0,0,0,0,0,0,1,0), #Nov 1960
                 c(144, 0,0,0,0,0,0,0,0,0,0,1)) #Dec 1960
                            
pred_ar1 <- predict(fit_ar1, n.ahead=12, newxreg=newxreg)$pred
pred_ar1 

err_ar1 <- airpass - pred_ar1
err_ar1

mape_ar1 <- mean(abs(err_ar1/airpass))*100
mad_ar1 <- mean(abs(err_ar1))
msd_ar1 <- mean(err_ar1^2)

c(mape05, mape23, mape50, mape011, mape_es, mape_ar1)
c(mad05, mad23, mad50, mad011, mad_es, mad_ar1)
c(msd05, msd23, msd50, msd011, msd_es, msd_ar1)

# Example 2. Prediction of log air passenger miles

airmiles <- scan('airmiles.txt', skip=3)
airmiles <- ts(log(airmiles), start=c(1960, 1), frequency=12)    
airmiles

x <- airmiles 

#from weeks 6 and 7 
fit2 <- arima(x, order=c(0,1,2), seasonal=list(order=c(0,1,1), period=12))

tsdiags(fit2)

par(mfrow=c(1,1))
resid2 <- resid(fit2) 
qqnorm(resid2)
qqline(resid2)

shapiro.test(resid2)

plot(airmiles)

#now repeat with 1970-77 data  

x <- window(airmiles, start=c(1970,1))  

fit2 <- arima(x, order=c(0,1,2), seasonal=list(order=c(0,1,1), period=12))

tsdiags(fit2)

par(mfrow=c(1,1))
resid2 <- resid(fit2) 
qqnorm(resid2)
qqline(resid2)

shapiro.test(resid2)

#now see if model is different for 1970-77 data 

par(mfrow=c(2,1))
 acf(diff(diff(x), lag=12), lag=50)
pacf(diff(diff(x), lag=12), lag=50)

fit3 <- arima(x, order=c(2,1,0), seasonal=list(order=c(0,1,1), period=12))
tsdiags(fit3)
 
par(mfrow=c(1,1))
resid3 <- resid(fit3) 
qqnorm(resid3)
qqline(resid3)

shapiro.test(resid3)

fit2$loglik; fit3$loglik

#now make predictions from end of 1976

x <- window(airmiles, start=c(1970,1), end=c(1976,12)) 

#fit model again 
fit2 <- arima(x, order=c(0,1,2), seasonal=list(order=c(0,1,1), period=12))

pred2 <- predict(fit2, n.ahead=12)$pred
err2 <- airmiles - pred2

mape2 <- mean(abs(err2/airmiles))*100
mad2 <- mean(abs(err2))
msd2 <- mean(err2^2)

#fit model again 
fit3 <- arima(x, order=c(2,1,0), seasonal=list(order=c(0,1,1), period=12))

tsdiags(fit3)
pred3 <- predict(fit3, n.ahead=12)$pred
err3 <- airmiles - pred3

mape3 <- mean(abs(err3/airmiles))*100
mad3 <- mean(abs(err3))
msd3 <- mean(err3^2)

# triple exponential smoothing

x <- window(airmiles, start=c(1970,1), end=c(1976,12)) 

es_add <- HoltWinters(x, seasonal="additive")

pred_es <- predict(es_add, n.ahead=12)

err_es <- airmiles - pred_es
err_es

par(mfrow=c(1,1))
plot(airmiles)
lines(pred_es, col="red")

mape_es <- mean(abs(err_es/airmiles))*100
mad_es <- mean(abs(err_es))
msd_es <- mean(err_es^2)

c(mape2, mape3, mape_es)
c(mad2, mad3, mad_es)
c(msd2, msd3, msd_es)

#decomposition method 

x <- window(airmiles, start=c(1970,1), end=c(1976,12)) 

decomp <- decompose(x, "additive")

plot(decomp)

x <- decomp$random
x <- window(x, c(1970,7), c(1976,6))
x

par(mfrow=c(2,1))
acf(x, 50); pacf(x, 50)

Box.test(x,  6, "Ljung-Box", fitdf=0)
Box.test(x, 10, "Ljung-Box", fitdf=0)
Box.test(x, 12, "Ljung-Box", fitdf=0)
Box.test(x, 18, "Ljung-Box", fitdf=0)
Box.test(x, 24, "Ljung-Box", fitdf=0)

acf(rnorm(100))
acf(rnorm(100))

decomp$trend

t <- 1:6
y <- decomp$trend[61:66]
plot(t, y, type="l")

pred <- predict(lm(y~t+I(t^2)), newdata=list(t=7:24))
pred

decomp$seasonal

pred <- pred[7:18] + decomp$seasonal[61:72]
pred=ts(pred, start=c(1977,1), frequency=12)

par(mfrow=c(1,1))
plot(airmiles)
lines(pred, col="red")

err_dc <- airmiles - pred
err_dc

mape_dc <- mean(abs(err_dc/airmiles))*100
mad_dc <- mean(abs(err_dc))
msd_dc <- mean(err_dc^2)

c(mape2, mape3, mape_es, mape_dc)
c(mad2, mad3, mad_es, mad_dc)
c(msd2, msd3, msd_es, msd_dc)

