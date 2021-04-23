#plot the chi-square density and draw some vertical lines
#in connection with 6.5 Examples 
#look at P-value and critical value  

df <- 10		#(number of) degrees of freedom

#plot the density as a function of x 
#over x between 0 and df*3 
#no need to compute function values, just define it 

plot(function(x) dchisq(x, df), 0, df*3, ylab="dchisq") 	

abline(h=0)		#the horizontal axis 

x <- 5.6107		#value of Ljung-Box statistic Q

#add vertical line at x, lty=2 (dotted)
lines(c(x, x), c(0, dchisq(x, df)), lty=2)

#find P-value corresponding to x
1-pchisq(x, df) 	#probability to the right of x (below density curve)

#probability to the left (pos=3)
text(x, dchisq(x, df)/2, sprintf("%4.2f",   pchisq(x, df)), pos=2)   

#probability to the right (pos=4) 
text(x, dchisq(x, df)/2, sprintf("%4.2f", 1-pchisq(x, df)), pos=4)   
x <- qchisq(0.95, df)
lines(c(x, x), c(0, dchisq(x, df)), lty=2)
text(x, dchisq(x, df)/2, sprintf("%4.2f",   pchisq(x, df)), pos=2)   
text(x, dchisq(x, df)/2, sprintf("%4.2f", 1-pchisq(x, df)), pos=4)   

#now do 10% cv, just duplicate the code - no need to write a 'function' 
qchisq(0.90, df)

x <- qchisq(0.90, df)
lines(c(x, x), c(0, dchisq(x, df)), lty=2)
text(x, dchisq(x, df)/2, sprintf("%4.2f",   pchisq(x, df)), pos=2)   
text(x, dchisq(x, df)/2, sprintf("%4.2f", 1-pchisq(x, df)), pos=4)   


#important: P-value=0.85 > alpha=0.05 => Q=5.6107 < cv=18.3070 
#use P-value if given, otherwise find cv or P-value 

#which significance level to use: 5% by default 

#when test is nonsignificant at both levels, 
#use larger of 0.05 and 0.10 (to make a stronger statement)
#nonsignificant at 10% => nonsignificant at 5% 

#when test is significant at both levels, 
#use smaller of 0.05 and 0.01
#significant at 0.01 => significant at 0.05

#unless (exam) question says otherwise 
 
