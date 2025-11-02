install.packages('tidyverse')
library(tidyverse)

school <- read.csv("D:/Sem 1/App stats and prob/school.csv/Credit_Risk7_final.csv")
summary(school)  #Summary of school data
summary(school$Height)
sd(school$Height)

#Recode 1 to male and 2 to female
school$gender2[school$Gender=="1"]<-"Male"
school$gender2[school$Gender=="2"]<-"Female"
head(school)

table(school$gender2)

barplot(table(school$gender2), main="Barchart of Gender", ylab="Count")
barplot(table(school$gender2), main="Barchart of Gender", xlab = "Gender", ylab="Frequency", ylim=c(0,12))
hist(school$Weight)

pie(table(school$gender2))

boxplot(school$Height, ylab="Height in inches")

boxplot(school$Age, ylab="Age in years")

summary(school$Age)

install.packages("e1071")
library(e1071)
skewness(school$Height, na.rm=FALSE, type=1)#type 1,2,3 are the different formulas
kurtosis(school$Height)

install.packages("skimr")
library(skimr)
boxplot(school$Height~school$gender2, ylab="Height in inches")

summary(school$Height[school$gender2=="Male"])
summary(school$Height[school$gender2=="Female"])

# survey data
Survey <- read.csv("D:/Sem 1/App stats and prob/Survey.csv", header=TRUE)
head(Survey)

Survey$gender2[Survey$Gender=="0"]<-"Male"
Survey$gender2[Survey$Gender=="1"]<-"Female"
head(Survey)

Survey$Athlete2[Survey$Athlete=="0"] <- "Not an athlete"
Survey$Athlete2[Survey$Athlete=="1"] <- "Is an athlete"
head(Survey)

#Chart for Gender
table(Survey$gender2)

barplot(table(Survey$gender2), main="Barchart of Gender", xlab = "Gender", ylab="Frequency", ylim=c(0,250))
pie(table(Survey$gender2))

#chart for Athlete
table(Survey$Athlete2)

barplot(table(Survey$Athlete2), main="Barchart of Gender", xlab = "Gender", ylab="Frequency", ylim=c(0,250))
pie(table(Survey$Athlete2))

# Continous variables
Survey$Height2 <- Survey$Height
Survey$Height2[Survey$Height2==9999] <- NA
summary(Survey$Height2)
summary(Survey$Height)
boxplot(Survey$Height2, main="Barplot for Height", ylab="height")
boxplot(Survey$Height2~Survey$Gender,xlab="Gender", ylab="Height", main="Height and Gender")

#Weight
summary(Survey$Weight)
Survey$Weight2 <- Survey$Weight
Survey$Weight2[Survey$Weight2==9999] <- NA
summary(Survey$Weight2)
summary(Survey$Weight)
boxplot(Survey$Weight2, main="Barplot for Weight", ylab="Weight")
boxplot(Survey$Weight2~Survey$Gender,xlab="Gender", ylab="Weight", main="Weight and Gender")
hist(Survey?Weight2)

