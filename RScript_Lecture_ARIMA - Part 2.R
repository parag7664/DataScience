## ARMA MODELS ##
## =========== ##

install.packages("sarima")
install.packages("astsa")
install.packages("ggplot2")
install.packages("forecast")
install.packages("tseries")
library(tseries)
library(ggplot2)
library(astsa)
library(forecast)
library(sarima)


ARIMA(p,d,q)
#===========

?arima.sim   
      
# AR(1)  --> ARIMA(1,0,0)
out1 = arima.sim(list(order = c(1, 0, 0), ar = 0.9), n = 100)
## Yt = c + 0.9 Yt-1 + et

# MA(1) = ARIMA(p=0, d=0. q=1)
out4 = arima.sim(list(order = c(0, 0, 1), ma = -0.5), n = 100)
acf2(out4)
## Yt = c + et -0.5et-1
out = arima.sim(list(order = c(0, 0, 1), ma = -0.7), n = 100)
acf2(out)

# AR(2)
out = arima.sim( list (order = c(2, 0, 0), ar = c(0.7, 0.2) ) , n = 100)
acf2(out)


# ARMA(1,1)
out6 = arima.sim(list(order = c(1, 0, 1), ar = 0.9, ma = -0.5),  n = 100)

autoplot(out6, main="ARMA(1,1) phi=0.9 q = -0.5")
par(mfrow=c(3,1))
plot(out1, type='l')
plot(out4, type='l')
plot(out6, type='l')

acf(out6, main="ARMA(1,1)");pacf(out6, main="ARMA(1,1)")
acf2(out6)

# ARMA(1,1) or AR(2)

## Series Simulation ==> find a model looks like my TS
?dev.off()
autoplot(out1, main="AR(1) phi=0.9")
autoplot(out4, main="MA(1) theta=-0.5")
autoplot(out6, main="AR(1) phi=0.9 theta=-0.5")

# ARIMA #
# ===== #

CO2dat = read.csv("D:/Sem 2/Time Series and factor analysis/lab/co2_mm_mlo.csv")
#library(forecast)
View(co21)
autoplot(co2, xlab="Years", ylab="CO2 conc. per 1M", main="CO2 month average")
co21=co2[1:120]
co21 = ts(co21) 
autoplot(co21)

decompose(co2)
autoplot(decompose(co2))

# There is TREND problem --> stationarity comprised
#autoplot(diff(co2))
dev.off()
par(mfrow=c(2,2))
plot(co2)
plot(diff(co2), main="D(CO2)")
abline( h=mean(diff(co2)), col="red", lwd=2)
autoplot(decompose(diff(co2)))

## MODEL IDENTIFICATION
#install.package(astsa)
library(astsa)
#dev.off()
#graphics.off()
acf2(out1, 48, main="ARIMA(1,0)")  ## Autoregressive AR(1)

# MA(1) = ARIMA(p=0, d=0. q=1)
out4 = arima.sim(list(order = c(0, 0, 1), ma = 0.8), n = 100)
## Yt = c + et -0.5et-1
acf2(out4,48, main="ARIMA(0,1)")  ## Moving Average MA(1)

acf2(out6, 48, main="ARIMA(1,1)")

# EXAMPLE - Data Transformation
# Trend by Diff and Regression
# preciom2
#preciom2 = #scan("C://Users//casio//OneDrive - UMH//Escritorio//CIT LECTURES//preciom2.txt", skip=6)
#preciom2 = scan("C://Users//MANY//Desktop//CIT LECTURES//TIME SERIES AND FA//Lectures//Lecture 3 NOV ARIMA//preciom2.txt", skip=6)
price_m2 = scan("D:/Sem 2/Time Series and factor analysis/lab/price_m2.txt", skip=2)
prec2 = ts(price_m2, freq=12, start=c(1987,1))  
dprec = diff(prec2)     # 1st diff
d2prec = diff(prec2, diff=2) # 2nd diff
d3prec = diff(prec2, diff=3) # 3rd diff
#
dev.off()
par(mfrow=c(2,3))
autoplot(prec2)
autoplot(dprec)
autoplot(d2prec)
plot(prec2, main="Price m2")
plot(dprec, main="D(Price)")
plot(d2prec, main="D2(Price)") 
plot(d3prec, main="D3(Price)") 
# ACF comparison
acf(prec2, main="Pricem2")
acf(dprec, main="D(Pricem2)")
acf(d2prec, main="D2(Pricem2)")
# Decompose and Regression using Trend and Seasonality
dec = decompose(prec2)
autoplot(dec)

ddec = decompose(dprec)
autoplot(ddec)

d2dec = decompose(d2prec)
autoplot(d2dec)
#ff = lm(d2prec ~ d2dec$trend + d2dec$seasonal )
#plot(d2prec)
#abline(ff, col=2)

# Regression line to the TS 
?lm
fit <- tslm(prec2 ~ trend)
#fit2 <- tslm(prec2 ~ trend + season )
summary(fit)
fr = fit$residuals
plot(fr, main="Residuals")

# Ho: B1 = 0 (slope) --> 2e-16 < p-value = 0.05 -->  reject null hyp
#plot(fit)
#plot.ts(prec2)
#abline(fit, col=2)

# Only Check - fit regression to 1st diff
dfit <- tslm(dprec ~ trend + season)
dfit2 <- tslm(dprec ~ trend )
summary(dfit)
dfr = dfit$residuals
plot(dfr, main="D(residuals)")

# Only Check - fit regression to 2nd diff
d2fit <- tslm(d2prec ~ trend + season)
d2fit2 <- tslm(d2prec ~ trend )
summary(d2fit)
d2fr = d2fit$residuals
plot(d2fr, main="D2(residuals)", type="p")
lines(d2fr, col=2)

# Fit Quadratic Regression
fitQ <- tslm(prec2 ~ trend + I(trend^2) )

summary(fitQ)
frQ = fitQ$residuals
plot(frQ, main="Quadratic Residuals")

# Fit Cubic Regression
fitC <- tslm(prec2 ~ trend + I(trend^2)+ I(trend^3) )

summary(fitC)
frC = fitC$residuals
plot(frC, main="Cubic Residuals")

# fit plots
par(mfrow=c(2,2))
dev.off()
plot(prec2, lwd=3, main="Linear and Quad & Cubic fits")
lines(fit$fitted, col=2, lwd=2)
lines(fitQ$fitted, col=3, lwd=2)
lines(fitC$fitted, col=4, lwd=2)

plot(fr, main="Residuals")
plot(frQ, main="Quadratic Residuals")
plot(frC, main="Cubic Residuals")   ## These residuals have AR shape??
# Decompose and check seasonality
autoplot(decompose(frC), main="Cubic Residuals")
par(mfrow=c(2,2))
plot(frC, main="Cubic Residuals") 
plot(diff(frC), main="dif(Cubic Res)") # first diff remove trend?
plot(diff((frC), differences = 12), main="dif12(Cubic Res)") # stabilize after seasonal diff in 12
# first diff and seasonal
plot(diff(diff(frC), differences = 12), main="dif(1&12(Cubic Res))") # stabilize after seasonal diff in 12

#plot(prec2, main="Linear and Quadratic TS fits")
#lines(fit$fitted.values, col=2)
#lines(fitQ$fitted.values, col=3)

# Just Checking Residuals with diff
dfitQ <- tslm(dprec ~ trend + I(trend^2) )

summary(dfitQ)
dfrQ = dfitQ$residuals
plot(dfrQ, main="Residuals")
# fit plots
plot(dprec, main="Linear and Quadratic fits 1st diff")
lines(dfit$fitted, col=2, lwd =2)
lines(dfitQ$fitted, col=3, lwd=2)

dfitQ2 <- tslm(dprec ~ trend + I(trend^2) + season )
lines(dfitQ2$fitted, col=4, lwd=2)
#
d2fitQ <- tslm(d2prec ~ trend + I(trend^2) )

summary(d2fitQ)
d2frQ = d2fitQ$residuals
plot(d2frQ, main="Residuals")
# fit plots
plot(d2prec, main="Linear and Quadratic fits 2nd diff")
lines(d2fit$fitted, col=2)
lines(d2fitQ$fitted, col=3)

# ACFs 
par(mfrow=c(2,3))
acf(prec2);acf(dprec);acf(d2prec)
# ACF residuals
acf(fr)
acf(frQ)
acf(frC)

# Forecast model
#autoplot(forecast(fit, h=20))
#autoplot(forecast(fit, h=20))
#autoplot(forecast(fitQ, h=20))
#autoplot(forecast(fitC, h=20))


#################################
#################################
## STOCK PRICE
## ==============
# Non-constant VAR Problem ==> LOG

stock = scan("D:/Sem 2/Time Series and factor analysis/lab/stock_price.txt", skip=2, dec=",")

# read , as . with --> dec=","
prec2 = ts(stock, freq=12, start=c(1991,1))  
dprec = diff(prec2)     # 1st diff
d2prec = diff(prec2, diff=2) # 2nd diff
# After diff for removing trend there is still VAR problem
autoplot(prec2, main="TS")
par(mfrow=c(2,3))
plot(prec2, main="TS")
plot(dprec, main="D(TS)")
plot(d2prec, main="D2(TS)")
# Take LOGS
Lprec = log(prec2)  
Ldprec = diff(Lprec)     # 1st diff
Ld2prec = diff(Lprec, diff=2) # 2nd diff
#par(mfrow=c(2,3))
plot(Lprec, main="log(TS)"); 
plot(Ldprec, main="D(log[TS])"); 
plot(Ld2prec, main="D2(log[TS])")

# ACFs de las series
dev.off()
par(mfrow=c(2,3))
plot(Lprec, main="log(TS)"); plot(Ldprec, main="D(log[TS])"); plot(Ld2prec, main="D2(log[TS])")
# ACF comparison
acf(Lprec); acf(Ldprec, h=30);acf(Ld2prec)
##
#ARIMA(p,d,q)(P,D,Q)s

#acf(prec2); acf(dprec);acf(d2prec)
# Decompose and Regression using Trend and Seasonality
dec1 = decompose(prec2)
autoplot(dec1)

dec = decompose(Lprec)
autoplot(dec)

ddec = decompose(Ldprec)
autoplot(ddec)

d2dec = decompose(Ld2prec)
autoplot(d2dec)
#ff = lm(d2prec ~ d2dec$trend + d2dec$seasonal )
#plot(d2prec)
#abline(ff, col=2)

# Regression line to the TS 
## DO THE TRANSF TO LOG>>>>IS NOT DONE YET
fit <- tslm(prec2 ~ trend + season)
fit2 <- tslm(prec2 ~ trend )
summary(fit)
fr = fit$residuals
plot(fr, main="Residuals")
#plot(fit)
#plot.ts(prec2)
#abline(fit, col=2)

dfit <- tslm(dprec ~ trend + season)
dfit2 <- tslm(dprec ~ trend )
summary(dfit)
dfr = dfit$residuals
plot(dfr, main="D(residuals)")

d2fit <- tslm(d2prec ~ trend + season)
d2fit2 <- tslm(d2prec ~ trend )
summary(d2fit)
d2fr = d2fit$residuals
plot(d2fr, main="D2(residuals)")

# Fit Quadratic
fitQ <- tslm(prec2 ~ trend + I(trend^2) )

summary(fitQ)
frQ = fitQ$residuals
plot(frQ, main="Residuals")
# fit plots
plot(prec2, main="Linear and Quadratic fits")
lines(fit$fitted, col=2)
lines(fitQ$fitted, col=3)
#plot(prec2, main="Linear and Quadratic TS fits")
#lines(fit$fitted.values, col=2)
#lines(fitQ$fitted.values, col=3)
dfitQ <- tslm(dprec ~ trend + I(trend^2) )

summary(dfitQ)
dfrQ = dfitQ$residuals
plot(dfrQ, main="Residuals")
# fit plots
plot(dprec, main="Linear and Quadratic fits 1st diff")
lines(dfit$fitted, col=2)
lines(dfitQ$fitted, col=3)

#
d2fitQ <- tslm(d2prec ~ trend + I(trend^2) )
summary(d2fitQ)
d2frQ = d2fitQ$residuals
plot(d2frQ, main="Residuals")
# fit plots
plot(d2prec, main="Linear and Quadratic fits 2nd diff")
lines(d2fit$fitted, col=2)
lines(d2fitQ$fitted, col=3)

# ACF residuals
acf(fr)
acf(dfr)

acf(frQ)
acf(d2fr)

# Fit cuadratic model
plot(forecast(fit2, h=20))
plot(forecast(fit, h=20))

######################################
######################################
###### ARIMA #########################
###### ===== #########################
######################################

# Packs: SARIMA & ASTSA
# Time-Series Fish catch ==> dataset : "rec"
# ============================
###
arima(x, order = c(0L, 0L, 0L),
      seasonal = list(order = c(0L, 0L, 0L),
                      period = NA),
      xreg = NULL, include.mean = TRUE,
      transform.pars = TRUE,
      fixed = NULL, init = NULL,
      method = c("CSS-ML", "ML", "CSS"), n.cond,
      SSinit = c("Gardner1980", "Rossignol2011"),
      optim.method = "BFGS",
      optim.control = list(), kappa = 1e6)
##
#dataset: "rec"
#1. Visualize Plot & Correlograms
autoplot(rec)
acf2(rec)

fit = arima(rec, order =c(2,0,0)) ## AR(2) = ARIMA(2,0,0)
fit

fit1 = arima(rec, order =c(1,0,0)) ## AR(1) = ARIMA(2,0,0)
fit1


fit3 = arima(rec, order =c(3,0,0)) ## AR(2) = ARIMA(2,0,0)
fit3

fit11 = arima(rec, order =c(1,0,1)) ## AR(1) = ARIMA(2,0,0)
fit11
#Intercept in the model is the MEAN 
#Yt - 61.8 = 1.35(Yt-1 - 61.8) - 0.46(Yt-2 - 61.8) + et

#la pesca q voy a coger este mes depende de mucha medida del mes anterior, y en forma
#negativa de la pesca de hace 2 meses...i.e. tardan 2 meses en crecer los peces...pq 
#hace 2 meses no habia peces pero ahora si por el ciclo de vida q lleva p.e.

############################
####  SEASONALITY  #########
####  ===========  #########
#install.packages("astsa")
library(astsa)
library(tseries)
## there are log & diff to transform into Stationary TS
## New problem --> SEASONALITY
# ARIMA(p,q,d)xSARIMA(P,Q,D)s

sarima(xdata, p, d, q, P = 0, D = 0, Q = 0, S = -1,
       details = TRUE, xreg=NULL, Model=TRUE,
       tol = sqrt(.Machine$double.eps),
       no.constant = FALSE)
p: AR order (must be specified)
d: difference order (must be specified)
q: MA order (must be specified)
P: SAR order; use only for seasonal models
D: seasonal difference; use only for seasonal models
Q: SMA order; use only for seasonal models
S: seasonal period; use only for seasonal models
##
dev.off()
model = sarima(rec, 2,0,0) ## only 3 param out of 7 
model2 = sarima(rec, 2,0,0,1,0,0,10) ## still problems
model3 = sarima(rec, 2,0,0,0,0,1,10) ##  still problems
model4 = sarima(rec, 1,1,0,P=0,D=0,Q=2,S=12)

?adf.test
?kpss.test
adf.test(rec, alternative="stationary", k=0)
kpss.test(rec)

### Tests ####
# ADF test for AR(1) process
x <- arima.sim(list(order = c(1,0,0),ar = 0.2),n = 100)
adf.test(x)
# ADF test for co2 data
autoplot(co2)
adf.test(co2)

# KPSS test for AR(1) process
x <- arima.sim(list(order = c(1,0,0),ar = 0.2),n = 100)
kpss.test(x)
# KPSS test for co2 data
kpss.test(co2)
adf.test(co2)

# Ljung
#?Box.test
#Box.test(resid(model4),type="Ljung",lag=20,fitdf=1)
# forecast 

fitco2 = auto.arima(co2)
sarima.for(co2, 24, 1,1,1, 1,1,2 ,12, plot.all=FALSE )

?sarima.for
sarima.for(rec, 12,  1,1,0,0,0,2,12, plot.all = FALSE)

library(forecast)
checkresiduals(rec)
checkresiduals(x)

############
###########

# US GNP SERIES
#US GNP Series:
#  US GNP (PIB)
#library(tseries)
#library(astsa)
#library(ggplot2)

data(gnp)
autoplot(gnp, main="Quarterly U.S. GNP from 1947(1) to 1991(1)")
acf2(as.vector(gnp), 50)
#Stationarity
adf.test(gnp, alternative="stationary", k=0)
kpss.test(gnp)

# First Order Difference(non Stationary TS)
autoplot(diff(gnp), main= "First Difference of U.S. GNP 1947(1)-1991(1)")
acf2(diff(gnp), main= "First Difference of U.S. GNP 1947(1)-1991(1)")

#Stationarity
adf.test(diff(gnp), alternative="stationary", k=0)
kpss.test(diff(gnp))


# First Diff and LOG to correct non-constant Variance 
gnpgr = diff(log(gnp)) # growth rate
autoplot(gnpgr, main="First difference of the U.S. log(GNP) data")
acf2(gnpgr, main="First difference of the U.S. log(GNP) data")

#Stationarity
adf.test(gnpgr, alternative="stationary", k=0)
kpss.test(gnpgr)##


#############################
##  AUTO.ARIMA  #############
##  ==========  #############

#install.packages("forecast")
#library(forecast)
auto.arima(x, d=NA, D=NA, max.p=5, max.q=5,
           max.P=2, max.Q=2, max.order=5, start.p=2,
           start.q=2, start.P=1, start.Q=1,
           stationary=FALSE,
           seasonal=TRUE,ic=c("aicc","aic", "bic"),
           stepwise=TRUE, trace=FALSE,
           approximation=(length(x)>100 | frequency(x)>12),
           xreg=NULL,test=c("kpss","adf","pp"),
           seasonal.test=c("ocsb","ch"),allowdrift=TRUE,
           lambda=NULL, parallel=FALSE, num.cores=NULL)
# library(forecast) # libro: https://otexts.com/fpp2/

## Try Different models to the Stationary series
ar.mod = Arima(log(gnp), order=c(1, 1, 0),
               seasonal = c(0, 0, 0),
               include.drift = TRUE)

?checkresiduals
checkresiduals(log(gnp))
checkresiduals(ar.mod)

ma.mod = Arima(log(gnp), order=c(0, 1, 1),
               seasonal = c(0, 0, 0),
               include.drift = TRUE)
checkresiduals(ma.mod)

arma11 = Arima(log(gnp), order=c(1, 1, 1),
               seasonal = c(0, 0, 0),
               include.drift = TRUE)
checkresiduals(arma11)
##
# If I use the command  auto.arima it will give me 
# the best possible ARIMA models that fit the data
# (based on smallest AIC)

?auto.arima

auto.arma11 = auto.arima(log(gnp),d=1,D=0,
                         seasonal=FALSE)

auto.arma11
checkresiduals(auto.arma11)
##
auto.free = auto.arima(log(gnp))

auto.free
checkresiduals(auto.free)


## PREDICCION

autoPred = forecast(auto.arma11, h=12)
autoPred
par(mfrow=c(1,2))
plot(autoPred)
# undo the LOG transformations
autoPred$mean = exp(autoPred$mean)
autoPred$lower = exp(autoPred$lower)
autoPred$upper = exp(autoPred$upper)
autoPred$x = exp(autoPred$x)
autoPred$fitted = exp(autoPred$fitted)
autoPred$residuals = exp(autoPred$residuals)
plot(autoPred)
#
#######################
## VALIDATION #########
#######################
n = length(gnp)
test = seq(n-12, n, by=1)
GNP_train = ts(gnp[ -test] , start = 1947, freq=4)
GNP_test = ts(gnp[ test ], freq =4, start = c(1947,1)  )

auto.arma11_train = auto.arima(log(GNP_train),d=1,D=0,
                         seasonal=FALSE)

auto.arma11_train
checkresiduals(auto.arma11_train)
## PREDICCION
autoPred_train = forecast(auto.arma11_train, h=12)
autoPred_train
# undo the LOG transformations
autoPred_train$mean = exp(autoPred_train$mean)
autoPred_train$lower = exp(autoPred_train$lower)
autoPred_train$upper = exp(autoPred_train$upper)
autoPred_train$x = exp(autoPred_train$x)
autoPred_train$fitted = exp(autoPred_train$fitted)
autoPred_train$residuals = exp(autoPred_train$residuals)
plot(autoPred_train)

library(ggplot2)

#(n -12)/4 
p <- ggplot(aes(x=x, y=y), data = GNP_train)
p <-  p + 
      geom_line() +
      geom_forecast(h=12, colour= "blue")

p <-  p + 
      geom_line(data=GNP_test, aes(x=x+((n -12)/4), y=y))
p


