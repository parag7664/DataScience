######################
# MODEL AIRPASSENGERS ##
########################

library(astsa)
library(tseries) # for ADF test
library(forecast) # auto.arima

data(AirPassengers) # load
class(AirPassengers)

AP = AirPassengers
AP

# AR"I"MA --> I take care of the trend --> linear trend d=1 --> ARIMA(p,1,q)
# log --> help the ARIMA

#1. PLOT THE TS  
autoplot(AP , main="Air Passengers")
summary(AirPassengers)
acf2(AirPassengers)

# Trend and Seasonality

dA = decompose(AirPassengers, "additive")
autoplot(dA)
dAm = decompose(AirPassengers, "multiplicative")
autoplot(dAm)

?mstl   ## periodicity
mstl(AirPassengers)
autoplot(mstl(AirPassengers))
autoplot(stlf(AirPassengers))

?ets  ## exponential smoothing mod  el auto
ets(AirPassengers)   ## auto.arima


# ARIMA MODEL 
# STATIONARITY TEST
adf.test(AP, alternative = "stationary",k = 0)
kpss.test(AP) # mean and trend  H0:stationary

?adf.test

# remove variability 
#autoplot(AP)
logAP = log(AP)
autoplot(logAP, main="log(AirPassengers)")
autoplot(decompose(logAP))

adf.test(logAP, alternative = "stationary",k = 0)

acf2(logAP)


# Remove trend 
dif_LAP= diff(logAP)
autoplot(dif_LAP, main = "D(log(AirPassengers))")

autoplot(decompose(dif_LAP))

adf.test(dif_LAP, alternative = "stationary",k = 0)
kpss.test((dif_LAP))

acf2(dif_LAP)


# Remove season 
?diff
D12dif_LAP = diff(dif_LAP, lag=1, differences = 12)

autoplot(D12dif_LAP)
autoplot(decompose(D12dif_LAP))

adf.test(D12dif_LAP)

pacf(D12dif_LAP)
acf(D12dif_LAP)

acf2(D12dif_LAP)
?acf2
# ACF / PACF stationary series

acf2(D12dif_LAP)

?auto.arima
# Auto.plot
fit_auto = auto.arima(AirPassengers)
fit_auto_log = auto.arima(log(AirPassengers))

#auto.arima(dlA)
fit_auto
fit_auto_log 

checkresiduals(fit_auto)
checkresiduals(fit_auto_log)

# Plot model found in auto
model_auto_AP = sarima(AP, 2,1,1,0,1,0,12)
model_auto_log_AP = sarima(log(AP), 0,1,1,0,1,1,12)

# prediction

predict_1 = predict(fit_auto, n.ahead = 2*12)
predict_log = predict(fit_auto_log, n.ahead = 2*12); predict_2 = exp(predict_log$pred)

predict_1 
predict_log; predict_2

ts.plot(AP, predict_2, lty=c(1,3) )
ts.plot(AirPassengers, predict_2, log ="y", lty=c(1,3))

# forecast


fore_1 = forecast(fit_auto, h = 2*12)
fore_log = forecast(fit_auto_log, h = 2*12); fore_2 = exp(predict_log$pred)

fore_1 
fore_log; fore_2

ts.plot(AP, fore_2, lty=c(1,3) )
ts.plot(AirPassengers, fore_2, log ="y", lty=c(1,3))

##
fore_log$mean = exp(fore_log$mean)
fore_log$lower = exp(fore_log$lower)
fore_log$upper = exp(fore_log$upper)
fore_log$x = exp(fore_log$x)
fore_log$fitted = exp(fore_log$fitted)
fore_log$residuals = exp(fore_log$residual)

dev.off()
plot(fore_log)

