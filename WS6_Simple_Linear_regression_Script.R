########################################
# Worksheet 6 Simple Linear Regression #
########################################


### plant data ####

library(tidyverse)
library(car)
# create data
Age<-c(2,2,3,	4,	4,	5,	6,	7,	8,	8)
Height<-c(4,	5,	9,	11,	12,	14,	17,	21,	22,	24)

# create a tibble
plant<-tibble(Age,Height)

## Examine the relationship between Age and Height
plot(Height ~ Age, data = plant)
cor(plant$Height, plant$Age)


#fit model
plant_model<-lm(Height ~ Age, data = plant)
## summary output
summary(plant_model)

## extract coefficients and residuals
plant_model$coefficients
plant_model$residuals
#summary ANOVA table
summary.aov(plant_model)

#plot regression line
windows(5,5)
plot(Height ~ Age)
abline(plant_model)


# 95% confidence interval for fitted value (mean) and predicted value (individual)
new <- data.frame(Age = 3.5)
predict(plant_model, new, interval = 'confidence', se.fit = T)
predict(plant_model, new, interval = 'prediction', se.fit = T)

## 95% confidence bands for mean and individual values

## create a sequence of values for the explanatory variable (x)
Age_grid = seq(min(Age), max(Age), by = 0.01)

## create a set of 95% confidence intervals at each point in Age_grid
dist_ci_band = predict(plant_model, 
                       newdata = data.frame(Age = Age_grid), 
                       interval = "confidence", level = 0.95)
## create a set of 95% prediction intervals at each point in Age_grid
dist_pi_band = predict(plant_model, 
                       newdata = data.frame(Age = Age_grid), 
                       interval = "prediction", level = 0.95) 

## plot scatter graph of Age against Height
plot(Height ~ Age, xlab = "Age (weeks)",  ylab = "Height (cm)",  pch  = 20,
     cex  = 0.75, ylim = c(min(dist_pi_band), max(dist_pi_band)))

## add regression line    
abline(plant_model)

## plot confodence and prediction bands
lines(Age_grid, dist_ci_band[,"lwr"], col = "blue", lwd = 1, lty = 2)
lines(Age_grid, dist_ci_band[,"upr"], col = "blue", lwd = 1, lty = 2)
lines(Age_grid, dist_pi_band[,"lwr"], col = "red", lwd = 1, lty = 3)
lines(Age_grid, dist_pi_band[,"upr"], col = "red", lwd = 1, lty = 3)

## diagnostics

windows(10,10)
par(mfrow = c(2,2)) 
plot(plant_model)

qqPlot(plant_model)

stdres <- rstandard(plant_model)

plot(stdres~fitted(plant_model), xlab = "Height (cm) (fitted values)", ylab =" standardised residuals")
abline(0,0)


###### Example 2  ##########
# In this example, a transformation is necessary to satisfy the assumptions of 
# linear regression

Decay <- read_tsv("Decay.txt")
windows(6,6)
# check the distribution of the amount variable
hist(Decay$amount)

# plot the relationship between the amount and time
windows(6,6)
plot(Decay$amount~Decay$time)

#fit a linear model to the amount variable  and examine the residuals
model1<- lm(amount~time, data=Decay)
summary(model1)
abline(model1)

windows(10,10)
par(mfrow = c(2,2)) 
plot(model1)

#examine the scatter plot and distribution of the transformed amount variable
plot(log(amount) ~ time, data = Decay)
hist(log(Decay$amount))
plot(log2(Decay$amount)~Decay$time)
plot(log10(Decay$amount)~Decay$time)


#fit a linear model to the transformed amount variable (log(amount)~time)
model2<- lm(log(amount)~time, data=Decay)
summary(model2)

# extracting coefficients from the output, the regression equation is
#  log(amount) =  4.547386 - 0.068528 x time

# to re-express the model in terms of the original variables take exponentials of both sides 
# exp(log(amount)) = exp(4.547386 - 0.068528 x time)
# amount = exp(4.547386)*exp(- 0.068528 x time)
# amount = 94.38536*exp(- 0.068528 x time)

# If 15 time steps have elapsed, then the predicted amount of organic material is 
# amount = 94.38536*exp(- 0.068528 x 15) = 33.76639


## Examine the residuals 
plot(model2)

#fit a non linear model to the variables (amount~time)
model3 <- nls(amount ~ a*exp(b*time), data = Decay, start = list(a=1, b=-0.1))
summary(model3)

# the non linear regression equation is amount ~ 108.135603*exp(-0.080186*time)
# after 15 time steps have elapsed:
# amount = 108.135603*exp(-0.080186*15) = 32.47907
#check using predict
newdata = data.frame(time = 15)
predict(model3, newdata)

## examine residuals
install.packages("nlstools")
library(nlstools)
windows(10,10)
plot(nlsResiduals(model3)) 


########## Create plots to compare the fit of the three models

#define a vector of data that can be used as the time variable when we create the fitted lines:
ts<- seq(0, 30, 0.02)
#turn the vector into a dataframe(this is required for the predict function)
ts<-data.frame(time=ts) 

# create a vector that contains the points fitted using the model1
line1<- predict(model1, newdata=ts) # linear model fitted to the original data
# create a vector that contains the points fitted using the model2
line2<-predict(model2, newdata=ts) # linear model fitted to the transformed data
# create a vector that contains the points fitted using the model3
line3<- predict(model3, newdata=ts) # nonlinear model fitted to the original data


#create the three plots in one window
windows(14,4)
par(mfrow=c(1,3))
#plot the points for the original data
plot(amount~time, data=Decay)
#draw the line from the fitted model1
lines(ts$time, line1)

#plot the points for the transformed data
plot(log(amount)~time, data=Decay)
#draw the line from the fitted model2
lines(ts$time, line2)

#plot the points fror the original data
plot(amount~time, data=Decay)
#draw the line from the fitted model3
lines(ts$time, line3)





