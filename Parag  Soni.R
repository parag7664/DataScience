

library(tidyverse)
library(here)
library(outliers)

## Import data
setwd("D:/Sem 2/statistical Data analysis/assessment 1")
mining <- read.csv("Mining4.csv")


#EDA
# Basic summary statistics
mining %>% group_by(method) %>% 
  summarise(mean = mean(copper), sd = sd(copper))


## Factorise 
mining$method<-as.factor(mining$method)


## plot the distribution of the copper for each method 
boxplot(mining$copper~mining$method, ylab = "Copper")

## check distributions of each variable
mining %>%
  filter(method == "A") %>%
  with(hist(copper))


#  Check outlier in the copper variable - Potential outlier not significant
grubbs.test(mining$copper)

#  Check outlier in the copper variable for the method where 
## the 99.28 copper where method is C.
grubbs.test(mining$copper[mining$method=="E"])

## Calculate the variance of the copper for each method type
mining %>% group_by(method) %>% 
  summarise( var_Copper = var(copper))

## test for equality of variances (homogeniety of variance assumption)
fligner.test(mining$copper~mining$method)


## Fit the ANOVA model
model_mining<- aov(mining$copper~mining$method) # define ANOVA model
summary(model_mining)      #view summary of ANOVA model (ANOVA table)
summary.lm(model_mining)   # view treatment effect estimates

#### Examine the pairwise effects  ###
TukeyHSD(model_mining)
plot(TukeyHSD(model_mining))

### Model validation plots ###
par(mfrow = c(1,1)) #split graphics window into 2 rows 2 columns
plot(model_mining)  # validation plots



########################################
library("readxl")
library(pwr)
library(asbio)

var(grades)
grades <- read_excel("grades3.xlsx")

df <-  data.frame(grades)


df %>% 
  summarise(mean = mean(grades), sd = sd(grades))

power.z.test(sigma = 55.42, n = 50, alpha = 0.05, effect = 0.5, test = "two.tail", strict = FALSE)


power.z.test(sigma = 1.35, power = 0.9, alpha = 0.05, effect = 0.6, test = "two.tail", strict = FALSE)
#effect size is 53
