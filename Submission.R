library(tseries)
library(ggplot2)
library(astsa)
library(forecast)

#Question 1
#simulate 

# AR(1)  --> ARIMA(1,1,0)
out1 = arima.sim(list(order = c(1, 1, 0), ar = 0.9), n = 100)
plot(out1, type='l')
acf2(out1)




#########################################################################################
Electric_Production = read.csv("D:/Sem 2/Time Series and factor analysis/lab/Electric_Production.csv")
Electric_Production_ts = ts(Electric_Production[,2], freq=12, start=c(1985,1))  
acf2(Electric_Production_ts)

View(Electric_Production_ts)
autoplot(Electric_Production_ts, xlab="Years", ylab="CO2 conc. per 1M", main="CO2 month average")
autoplot(Electric_Production_ts)

dA = decompose(Electric_Production_ts, "additive")
autoplot(dA)
dAm = decompose(Electric_Production_ts, "multiplicative")
autoplot(dAm)


mstl(Electric_Production_ts)
autoplot(mstl(Electric_Production_ts))


# by looking we have trend and seasonal component both so will use Holts winter
?hw
fit_hwM = hw(Electric_Production_ts, h=12, damped = FALSE, seasonal = "additive" )
fit_hwDM = hw(Electric_Production_ts, h=12, damped = TRUE, seasonal = "multiplicative")

plot(fit_hwM)
plot(fit_hwDM)

accuracy(fit_hwDM)
accuracy(fit_hwM)

summary(fit_hwDM)
summary(fit_hwM)


# ARIMA MODEL 
# STATIONARITY TEST
autoplot(decompose(Electric_Production_ts))

adf.test(Electric_Production_ts, alternative = "stationary",k = 0)
kpss.test(Electric_Production_ts) # mean and trend  H0:stationary

logAP = log(Electric_Production_ts)
autoplot(logAP, main="log(Electric_Production_ts)")
autoplot(decompose(logAP))

adf.test(logAP, alternative = "stationary",k = 0)
acf2(logAP)


# Remove trend 
dif_LAP= diff(logAP)
autoplot(dif_LAP, main = "D(log(Electric_Production_ts))")
autoplot(decompose(dif_LAP))

adf.test(dif_LAP, alternative = "stationary",k = 0)
kpss.test((dif_LAP))

acf2(dif_LAP)

# Remove season 
D12dif_LAP = diff(dif_LAP, lag=1, differences = 12)
autoplot(D12dif_LAP)
autoplot(decompose(D12dif_LAP))
acf2(D12dif_LAP)


# Auto.plot
fit_auto = auto.arima(Electric_Production_ts)
fit_auto_log = auto.arima(log(Electric_Production_ts))

#auto.arima(dlA)
fit_auto
fit_auto_log 

checkresiduals(fit_auto)
checkresiduals(fit_auto_log)

# Plot model found in auto
# model_auto_AP = sarima(Electric_Production_ts, 2,1,1,0,1,1,12)
# model_auto_log_AP = sarima(log(Electric_Production_ts), 1,1,1,1,1,2,12)

# prediction

predict_1 = predict(fit_auto, n.ahead = 2*12)
predict_log = predict(fit_auto_log, n.ahead = 2*12); 
predict_2 = exp(predict_log$pred)

predict_1 
predict_log; 
predict_2

ts.plot(Electric_Production_ts, predict_2, lty=c(1,3) )
ts.plot(Electric_Production_ts, predict_2, log ="y", lty=c(1,3))

# forecast


fore_1 = forecast(fit_auto, h = 2*12)
fore_log = forecast(fit_auto_log, h = 2*12); fore_2 = exp(predict_log$pred)

fore_1 
fore_log; fore_2

ts.plot(Electric_Production_ts, fore_2, lty=c(1,3) )
ts.plot(Electric_Production_ts, fore_2, log ="y", lty=c(1,3))

##
fore_log$mean = exp(fore_log$mean)
fore_log$lower = exp(fore_log$lower)
fore_log$upper = exp(fore_log$upper)
fore_log$x = exp(fore_log$x)
fore_log$fitted = exp(fore_log$fitted)
fore_log$residuals = exp(fore_log$residual)

dev.off()
plot(fore_log)