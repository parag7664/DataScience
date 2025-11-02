##Time series Questions
library(zoo)
library(ggplot2)
#install.packages("remotes")
#remotes::install_github("vqv/ggbiplot")
library(devtools)
library(forecast)
library(imputeTS)
library(astsa)
library(tseries) 
install.packages("imputeTS")
library("imputeTS")
packageVersion("imputeTS")
#Loading the dataset

setwd("D:/Sem 2/Time Series and factor analysis/assignment 2")
household_power_df<-read.table("household_power_consumption dataset.txt" , header=T, sep=";")
##To get the monthly average global_active power
monthly_Power_consumption<-household_power_df[,1:3]
monthly_Power_consumption

##Check Missing Values
is.na(monthly_Power_consumption$Global_active_power)
missing_value <- monthly_Power_consumption == "?"
is.na(monthly_Power_consumption)[missing_value] <- TRUE 

##Handling of missing value
monthly_Power_consumption$Global_active_power<- as.numeric(monthly_Power_consumption$Global_active_power)
na_interpolation(monthly_Power_consumption$Global_active_power)

#Taking the time stamp
monthly_Power_consumption$timestamp<-paste(monthly_Power_consumption$Date, monthly_Power_consumption$Time)

## Monthly average of energy consumed 
avg_power_consumption_per_month <- aggregate(monthly_Power_consumption,by=list(as.yearmon(monthly_Power_consumption$timestamp,
                                                                "%d/%m/%Y %H:%M:%S")), FUN=mean,na.rm=TRUE)

##Taking required column
Global_active_power_per_month <- avg_power_consumption_per_month[c(1,4)]

##preliminary descriptive analysis
summary(Global_active_power_per_month$Global_active_powe)
str(Global_active_power_per_month)
plot(Global_active_power_per_month, xlab="time", ylab="monthly active power consumption",type= "l", main="Global_active_power_per_month Time series plot")
boxplot(Global_active_power_per_month$Global_active_power, main="Boxplot of monthly active power consumption")
summary(Global_active_power_per_month)


#Converting data to timeseries
power_df<- Global_active_power_per_month[2]
power_ts<- ts(power_df, start = c(2006,12) , freq = 12)
power_ts

#splitting data

dat_train = subset(power_ts, Class == 'Train')
dat_test = subset(power_ts, Class == 'Test')

nrow(dat_train); nrow(dat_test)

##Decompose the Global_active_power_per_month
autoplot(decompose(power_ts)) 

##decomposition with additive
autoplot(decompose(power_ts, type="additive"))

##decomposition with multiplicative
autoplot(decompose(power_ts, type="multiplicative"))

##Periodicity
autoplot(mstl(power_ts))

##############################################################################################

#Time series modelling

##Classical Methods
##Additive Model
power_hw_additive_model <- hw(power_ts, h=12, damped = FALSE)
plot(power_hw_additive_model)
summary(power_hw_additive_model)

##multiplicative model
power_hw_multiplicative_model <- hw(power_ts, h=12, damped = FALSE, seasonal = "multiplicative" )
plot(power_hw_multiplicative_model)
summary(power_hw_multiplicative_model)



#stationary test:

adf.test(power_ts, alternative = "stationary",k = 0)

##ADF hypothesis
# H0:The time series is not stationary.
# Ha : The time series is stationary.

#as p value is >0.05 hence this time series is not stationary

# Remove trend and seasonality to make it stationary

#Remove variability
logdata <- log(power_ts)
autoplot(logdata, main="log(Power_Consumption)")

#Remove trend
dif_LAP<- diff(logdata)
autoplot(dif_LAP, main = "D(log(Power_Consumption))")

autoplot(decompose(dif_LAP))

#remove season
dif_ses = diff(dif_LAP,lag=1, differences = 12)
autoplot(dif_ses)
autoplot(decompose(dif_ses))

adf.test(dif_ses, alternative = "stationary",k = 0)

acf(dif_ses)
pacf(dif_ses)

#ARIMA model
fit_auto_arima = auto.arima(dif_ses, ic="aic", trace=TRUE)
fit_auto_arima
checkresiduals(fit_auto_arima)



## other models to the Stationary series
ar.mod = Arima(dif_ses, order=c(0, 0, 0),
               seasonal = c(0, 0, 0),
               include.drift = TRUE)

checkresiduals(ar.mod)
summary(ar.mod)

ma.mod = Arima(dif_ses, order=c(4, 0, 0),
               seasonal = c(0, 0, 0),
               include.drift = TRUE)
checkresiduals(ma.mod)
summary(ma.mod)




########## Forecasting Additive, Multiplicative and ARIMA Model ################

fcast_additive <- forecast(power_hw_additive_model, h = 12)
plot(fcast_additive, main="Prediction on Additive Model")
fcast_additive


fcast_multiplicative <- forecast(power_hw_multiplicative_model, h = 12)
plot(fcast_multiplicative, main="Prediction on Multiplicative Model")
fcast_multiplicative


mypowerforecast = forecast(fit_auto_arima, h=12)
plot(mypowerforecast)
mypowerforecast

