data= read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
data = ts(data[,2],start = c(2003,1),frequency = 12)
plot(data, xlab='Years', ylab = 'Tractor Sales')
plot(diff(data),ylab='Differenced Tractor Sales') #to check d(differencing) for checking wether it is stationary or not
plot(log10(data),ylab='Log (Tractor Sales)')
plot(diff(log10(data)),ylab='Differenced Log (Tractor Sales)') #diiferencing after log transformation
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF Tractor Sales')
pacf(ts(diff(log10(data))),main='PACF Tractor Sales')
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)# we can pass p,d,q in arima
summary(ARIMAfit)
par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 36)#predicting 36mnths
pred
plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue')#doing antilog 
lines(10^(pred$pred+2*pred$se),col='orange')#orange line is 95% confidence interval
lines(10^(pred$pred-2*pred$se),col='orange')
par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')#refer to ucanalytics.com step by step guide to forecasting throgh arima
