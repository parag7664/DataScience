#Question 1

library(tseries)
library(ggplot2)
library(astsa)
library(forecast)
library(tidyverse)
library("dplyr")
install.packages("summarytools")
library(summarytools)
library(VIM)
install.packages("moments")
library(moments)

#whether the variable body, is of use in predicting the value of brain. 
setwd("D:/Sem 2/statistical Data analysis/assignment 2")
weight_df = read.csv("weightsF.csv")
str(weight_df)
attach(weight_df)

# Summarising data
summary(weight_df)

plot(weight_df$brain ~ weight_df$body, main = "Body and Weight Scatter Plot")

# hsistogram
hist(weight_df$body, main = "Histogram of Body")
skewness(body)
hist(weight_df$brain, main = "Histogram of Brain")
skewness(brain)

#box plot
boxplot(weight_df$brain)
boxplot(weight_df$body)

#correlation coeff
cor(weight_df$brain, weight_df$body)

# fitting model
model<-lm(weight_df$brain ~ weight_df$body, data = weight_df)
summary(model)
summary.aov(model)

model$coefficients
model$residuals

#windows(5,5)
plot(weight_df$brain ~ weight_df$body)
abline(model)

#To correct the skew we can try a log transformation:
log_body<-log10(weight_df$body)
log_brain<-log10(weight_df$brain)

plot(log_brain~log_body)

hist(log_body, main = "Histogram of Body")
hist(log_brain, main = "Histogram of Brain")

cor_weight <- cor(log_brain, log_body)
print(paste("correlation between body and Brain", round(cor_weight, 4)))


#fit model
weight_model<-lm(log10(weight_df$brain) ~ log10(weight_df$body), data = weight_df)
## summary output
summary(weight_model)

## extract coefficients and residuals
weight_model$coefficients
weight_model$residuals
#summary ANOVA table
summary.aov(weight_model)

plot(log10(weight_df$brain) ~ log10(weight_df$body), xlab = "brain", ylab = "bdoy", main="Brain Vs Body")
abline(weight_model)
View(weight_df)

confint(weight_model, level=0.95)


## diagnostics

windows(10,10)
par(mfrow = c(2,2)) 
plot(weight_model)
install.packages(car)
library(car)

qqPlot(weight_model, ylab= 'Studenized residuals', main ='Q-Q Plot')

stdres <- rstandard(weight_model)

plot(stdres~fitted(weight_model), xlab = "Brain", ylab =" standardised residuals", main = "Q-Q plot")
abline(0,0)

#### ci BAND PLOT ##
#create a sequence of values for the explanatory variable (x)
#do i need sequence of values for both the variables????
brain_grid = seq(min(brain), max(brain), by = 0.00001)
brain_grid

body_grid = seq(min(weight_df$body), max(weight_df$body), by = 0.00001)
body_grid

dist_pi_band = predict(weight_model, newdata = data.frame(body_x= body_grid),
                       interval = "prediction", level = 0.95) 
dist_pi_band

## plot scatter graph of Age against Height
plot(log10(weight_df$brain) ~ log10(weight_df$body), xlab = "Brain",  ylab = "Body",  pch  = 20,
     cex  = 0.75, ylim = c(min(dist_pi_band), max(dist_pi_band)))


## add regression line    
abline(weight_model)

## plot confidence and prediction bands

lines(body_grid, dist_pi_band[,"lwr"], col = "red", lwd = 1, lty = 3)
lines(body_grid, dist_pi_band[,"upr"], col = "red", lwd = 1, lty = 3)
