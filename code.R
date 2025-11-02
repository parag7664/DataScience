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
plot(weight_df$brain ~ weight_df$body)

hist(weight_df$body, main = "Histogram of Body")
hist(weight_df$brain, main = "Histogram of Brain")

cor_weight <- cor(weight_df$brain, weight_df$body)
print(paste("correlation between amino and folate", round(cor_weight, 4)))


#fit model
weight_model<-lm(weight_df$brain ~ weight_df$body, data = weight_df)
## summary output
summary(weight_model)

## extract coefficients and residuals
weight_model$coefficients
weight_model$residuals
#summary ANOVA table
summary.aov(weight_model)

plot(weight_df$brain ~ weight_df$body, xlab = "folate", ylab = "amino", main="amino vs folate")
abline(weight_model)
View(amino_data)

confint(weight_model, level=0.95)


## diagnostics

windows(10,10)
par(mfrow = c(2,2)) 
plot(weight_model)
install.packages(car)
library(car)

qqPlot(weight_model, ylab= 'Studenized residuals', main ='Q-Q Plot')

stdres <- rstandard(weight_model)

plot(stdres~fitted(weight_model), xlab = "Folate (μg/L)", ylab =" standardised residuals", main = "Q-Q plot")
abline(0,0)

#### ci BAND PLOT ##
brain_grid = seq(min(brain), max(brain), by = 0.1)
brain_grid

body_grid = seq(min(body), max(body), by = 0.1)
body_grid

dist_pi_band = predict(weight_model, newdata = data.frame(folate = brain_grid, body_grid),
                       interval = "prediction", level = 0.95) 
dist_pi_band
## plot scatter graph of Age against Height
plot(amino_data$amino ~ log(amino_data$folate), xlab = "folate",  ylab = "amino",  pch  = 20,
     cex  = 0.75, ylim = c(min(dist_pi_band), max(dist_pi_band)))
amino_data

## add regression line    
abline(weight_model)

## plot confidence and prediction bands

lines(folate_grid, dist_pi_band[,"lwr"], col = "red", lwd = 1, lty = 3)
lines(folate_grid, dist_pi_band[,"upr"], col = "red", lwd = 1, lty = 3)

##########################################################################################
# model is : y = -0.75 +0.93*body beta1
# R-squared:  0.4793 = 47% of increase or decrease in brain by body positive relation

confint(model, level = 0.95) #weight_df$body  0.6981915  1.1692235

#b1 is not = 0 so we reject null hypothesis - H0 - there is no linear realtionship
summary.aov(model)
attach(weight_df)

#q.e

body_grid = seq(min(body), max(body), by = 0.05)                  
body_grid

dist_pi_band = predict(model, 
                       newdata = data.frame(body1 = body_grid), 
                       interval = "prediction", level = 0.95)
newdata
dist_pi_band

plot(brain ~ body,  xlab = "Age (weeks)",  ylab = "Height (cm)",
     pch  = 20, cex  = 0.75, ylim = c(min(dist_pi_band), max(dist_pi_band)))

abline(model)

lines(body_grid, dist_pi_band[,"lwr"], col = "red", lwd = 1, lty = 5)
lines(body_grid, dist_pi_band[,"upr"], col = "red", lwd = 1, lty = 5)


??stepAIC

## diagnostics

par(mfrow = c(2,2)) 
plot(blood_model1)

dev.off()

qqPlot(blood_model1)

# Standard Reidual
stdres <- rstandard(blood_model1)

plot(stdres~fitted(blood_model1), xlab = "Amino Level (fitted values)", ylab =" standardised residuals", main="standardized residuals against the fitted values ")
abline(0,0)

#calculate studentized residuals
stud_resids <- studres(blood_model1)

#plot predictor variable vs. studentized residuals
plot(blood_data$folate, stud_resids,  ylab='Studentized Residuals', xlab='Folate Level', main="Studentized residuals against the fitted values ") 

#add horizontal line at 0
abline(0, 0)



#14, 75, 43, 85

blood_data[c(14,43,75,85),]

