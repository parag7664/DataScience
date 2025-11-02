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

View(weight_df)
# Summarising data
summary(weight_df)

plot(weight_df$brain ~ weight_df$body, main = "Body and Weight Scatter Plot")

# hsistogram
hist(weight_df$body, main = "Histogram of Body", xlab = "Body")
skewness(body)
kurtosis(body)

hist(weight_df$brain, main = "Histogram of Brain", xlab = "Brain")
skewness(brain)
kurtosis(brain)


#correlation coeff
# cor(weight_df$brain, weight_df$body)
# 


#To correct the skew we can try a log transformation:
log_body<-log10(weight_df$body)
log_brain<-log10(weight_df$brain)

#box plot
boxplot(log_brain, main = "Boxplot of Brain")
boxplot(log_body, main = "Boxplot of Body")


plot(log_brain~log_body)

hist(log_body, main = "Histogram of Transformed Body")
hist(log_brain, main = "Histogram of Transformed Brain")

cor_weight <- cor(log_brain, log_body)
print(paste("correlation between body and Brain", round(cor_weight, 4)))


#fit model
weight_model<-lm(log10(brain) ~ log10(body), data = weight_df)
## summary output
summary(weight_model)

summary.aov(weight_model)

## extract coefficients and residuals
weight_model$coefficients
weight_model$residuals
#summary ANOVA table
summary.aov(weight_model)

plot(log10(weight_df$brain) ~ log10(weight_df$body), xlab = "bdoy", ylab = "brain", main="Brain Vs Body")
abline(weight_model)
View(weight_df)

confint(weight_model, level=0.95)

# model without transformation
# fitting model
model<-lm(weight_df$brain ~ weight_df$body, data = weight_df)
summary(model)
summary.aov(model)

par(mfrow = c(2,2)) 
plot(model, main = "model without transformation")

abline(weight_model)


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
par(mfrow = c(1,1))
#### ci BAND PLOT ##
#create a sequence of values for the explanatory variable (x)
#do i need sequence of values for both the variables????
brain_grid = seq(min(weight_df$brain), max(weight_df$brain), by = 0.001)
brain_grid

body_grid = seq(min(weight_df$body), max(weight_df$body), by = 0.001)
body_grid

dist_pi_band = predict(weight_model,newdata = data.frame(body = body_grid),
                       interval = "prediction", level = 0.95) 
dist_pi_band

## plot scatter graph
plot(log10(weight_df$brain) ~ log10(weight_df$body), xlab = "Brain",  ylab = "Body",  pch  = 20,
     cex  = 0.75, ylim = c(min(dist_pi_band), max(dist_pi_band)))


## add regression line    
abline(weight_model)

## plot confidence and prediction bands

lines(log10(body_grid), dist_pi_band[,"lwr"], col = "red", lwd = 1, lty = 3)
lines(log10(body_grid), dist_pi_band[,"upr"], col = "red", lwd = 1, lty = 3)
