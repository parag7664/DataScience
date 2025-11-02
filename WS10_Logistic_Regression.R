## Worksheet 10

library(faraway)
library(tidyverse)
orings<-orings

y<-cbind(orings$damage, 6-orings$damage)

##create Binary response variable to use in logistic regression model
Binary<-orings$damage
Binary[orings$damage == 0] <- 0
Binary[orings$damage > 0] <- 1

## plot data
windows(5,5)
plot(Binary~orings$temp,xlim=c(50,90))

##create logistic regression model
Ch_model_Binary<-glm(Binary~orings$temp, family=binomial)
summary(Ch_model_Binary)

## plot logistic regression model
x<-seq(25, 85, 1)
lines(x,ilogit(15.04-0.23*x))

## model checking
windows(10,10)
par(mfrow=c(2,2))
plot(Ch_model_Binary)

## count proportion of correct predictions
pred<-predict(Ch_model_Binary)
pred_prob<-exp(pred)/(1+exp(pred))
pred<-predict(Ch_model, type="response")

 pred_binary <- pred_prob
 pred_binary [pred_binary < 0.5] <- 0
 pred_binary [pred_binary > 0.5] <- 1

diff<- Binary-pred_binary
1-sum(abs(diff))/length(pred_binary)

### Analysis for challanger data - response variable is proportion of O-rings damaged.

plot(orings$damage/6~orings$temp, xlim=c(30,90), ylim=c(0,1), xlab="Temperature", ylab="p")

#create the matrix containing the number of successes and the number of failures
y<-cbind(orings$damage, 6-orings$damage)

#fit model
Ch_model_prop<-glm(y~orings$temp, family=binomial)
summary(Ch_model_prop)

## plot the fitted model
x<-seq(25, 85, 1)
lines(x,ilogit(11.66-0.22*x),type="l")



#### Worksheet 10 #####  insect data

insect<-read_tsv("genderratio.txt")

#scatterplot showing the proportion of males vs population density
# Note that as the density increases so does the proportion of males. 

plot(insect$males/insect$density~insect$density, xlim=c(-100,1000), xlab="Density", ylab="proportion male")
#create the response variable - vector containing the number of males and
# the number of females.
y<-cbind(insect$males,insect$females)

# model the proportion of males as a function of density
model<-glm(y~insect$density,binomial)
summary(model)

# create a vector containing a sequence of densities from 0 to 1000
z<-seq(0, 1000, 1)
lines(z,ilogit(0.0807368+0.0035101*z))

# examine the distribution of the population density variable - note the skew
hist(insect$density)
# try a log transformation
hist(log(insect$density))

#fit model using transformed data
model1<-glm(y~log(insect$density),binomial)
summary(model1)
plot(insect$males/insect$density~log(insect$density), xlim=c(-0,8), xlab="log(Density)", ylab="proportion male")
lines(z,ilogit(-2.65927+0.6941*z))



library(faraway)
library(tidyverse)
sexratio<-read_tsv("sexratio.txt")

#plot the data
plot(sexratio$males/sexratio$density~sexratio$density, xlim=c(-100,1000), xlab="Density", ylab="proportion male")

#create the proportional response variable
y<-cbind(sexratio$males,sexratio$females)

# fit the logistic regression model (proportion ~ population density)
model<-glm(y~sexratio$density,binomial)
summary(model)

# plot the model
z<-seq(-700, 1000, 1)
lines(z,ilogit(0.0807368+0.0035101*z))

# examine distribution of population density
hist(sexratio$density)
# examine the distribution of the log of population density
hist(log(sexratio$density))

# fit the logistic regression model (proportion ~ log(population density))
model1<-glm(y~log(sexratio$density),binomial)
summary(model1)

#plot fitted model
plot(sexratio$males/sexratio$density~log(sexratio$density), xlim=c(-0,8), xlab="log(Density)", ylab="proportion male")
lines(z,ilogit(-2.65927+0.6941*z))

# Can you interpret the coefficient associated with the log of the 
# population density?
# A unit increase in the log(population density) will increase the predicted 
# log of the odds by 0.69 and will increase the odds by a factor of 
# exp(0.6941)= 2.001907

# Since the p value is <0.01 we can conclued at the 1% significance level that
# the proportion of males is associated with the log(population density).

# compare the fitted model to the Null model directly using the residual deviance 
# of the fitted model and the Null model
pchisq(71.1593-5.6739,1, lower=F)

# compare the fitted model to the Null model using the anova function to perform 
# a likelihood ratio test (identical test statistic, identical result)
Null_model<-glm(y~1,binomial)
anova(model1, Null_model, test="Chisq" )

