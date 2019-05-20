###ARMA 
### Fitting af ARMA til X_t
auto.arima(X_t,d=0, stepwise = F, approximation = F)#p=1 og q=2
fitarma <- arima(X_t,order=c(1,0,2),include.mean = F)

acf(fitarma$residuals),lag.max = 100)
pacf(fitarma$residuals)
