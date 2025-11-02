# Regression Tutorial & Lab Exercise Sheet 1

# Q1
# Read in the Edu_salary.csv dataset.
# Ensure that your working directory in RStudio contains the dataset.

salary_data<-read.csv("Edu_salary.csv")
View(salary_data)
names(salary_data)
attach(salary_data)


# (a) Scatter plots
plot(Years,Salary,xlab="Years of Employment",ylab="Salary (???000's)")
plot(Rating,Salary,xlab="Rating (out of 100)",ylab="Salary (???000's)")

# (b)
YearsModel<-lm(Salary~Years)
RatingModel<-lm(Salary~Rating)

par(mfrow=c(2,1))
plot(Years,Salary,xlab="Years of Employment",ylab="Salary (???000's)")
abline(YearsModel)

plot(Rating,Salary,xlab="Rating (out of 100)",ylab="Salary (???000's)")
abline(RatingModel)

# (c)
cor(Salary,Years)
cor(Salary,Rating)

summary(YearsModel)
summary(RatingModel)

### Coefficient Standard Error 
# This measures the average amount that the  coefficient estimates vary 
# from the actual average value of our  response variable. 
# It is the the standard deviation of the coefficient.
# This can be used for computing confidence intervals. 

### t-statistic 
### This is the coefficient divided by the standard error. 
### In general, we want our coefficients to have large t-statistics, because it 
### indicates that our standard error is small in comparison to our coefficient.

### p-value
# A small p-value indicates that it is unlikely we will observe a relationship 
# between the predictor (speed) and response (dist) variables due to chance. 

### The Residual Standard Error is a measure of the quality of a linear regression fit. 
# we want the smallest residual standard error possible, because that means our 
#### model's prediction line is very close to the actual values, on average.

### F-statistic

# The null hypothesis is that the coefficients for all of the variables in 
# your model are zero. The alternative hypothesis is that at least one of them is not zero. 
# A larger F-statistic indicates that the null hypothesis should be rejected.
# A p-value below 0.05 indicates that you have at least one coefficient in your 
# model that is not zero. 

# (d)
predict(YearsModel,data.frame(Years = 10))  
predict(RatingModel,data.frame(Rating = 50))  

# (e) 
# Create model with all predictors included - multiple linear regression!
SalaryModel.all <- lm(Salary~Years+Rating)
SalaryModel.all

summary(SalaryModel.all) 
predict(SalaryModel.all,data.frame(Years=10,Rating=50))  


# Residual plot (not asked)
# Residuals are the difference between the actual observed response values 
# and the values predicted by the model. We should observe a symmetrical 
# distribution across these residuals with a the median value zero.
Salary.resid<-resid(SalaryModel.all)
plot(Salary,Salary.resid)
abline(0,0)

# Q2 

tyre_data<-read.csv("tyrepressure.csv")
View(tyre_data)
names(tyre_data)
head(tyre_data)
attach(tyre_data)

# (b)
par(mfrow=c(1,1))
plot(TyrePressure,mpg,xlab="Pressure (PSI)",ylab="Miles per Gallon")

# (c)
tyre_model<-lm(mpg~TyrePressure)
plot(TyrePressure,mpg,xlab="Pressure (PSI)",ylab="Miles per Gallon")
abline(tyre_model)

predict(tyre_model,data.frame(TyrePressure=32)) 

# Residuals and residual plot
mpg.resid<-resid(tyre_model)
plot(mpg,mpg.resid,xlab="Pressure",ylab="Residuals")
abline(0, 0)  

# (d)
cor(TyrePressure,mpg)
summary(tyre_model)

# (e) 
TyrePressure2<-TyrePressure^2 # quadratic independent variable
quad.tyre.model<-lm(mpg~TyrePressure+TyrePressure2) # quadratic model
summary(quad.tyre.model)

# To plot we need to create a new data set of x-vales
pressure_values<-seq(25,40,0.1)
predict_mpg<-predict(quad.tyre.model,
                     list(TyrePressure=pressure_values,
                          TyrePressure2=pressure_values^2))

plot(TyrePressure,mpg)
lines(pressure_values,predict_mpg)
                     
# predict mpg when pressure=32 using quadratic model
predict(quad.tyre.model,data.frame(TyrePressure=32,TyrePressure2=32^2))

###############################################################

# Q3
blood.data<-read.csv("SBP.csv")
names(blood.data)
View(blood.data)

# (a) Recode gender variable to 0,1 install.packages("dplyr") 
# tools for working with data frames

# library(dplyr)
blood.data$Gender<-ifelse(blood.data$Gender=="F",0,1) 
View(blood.data)
attach(blood.data)

# (b) 
lin.blood.model.age <- lm(SBP~Age)
summary(lin.blood.model.age)
lin.blood.model.weight <- lm(SBP~Weight)
summary(lin.blood.model.weight)

plot(Age,SBP)
abline(lin.blood.model.age)
plot(Weight,SBP)
abline(lin.blood.model.weight)

predict(lin.blood.model.weight,data.frame(Weight=190))
predict(lin.blood.model.age,data.frame(Age=80))

# (c)
t <- table(Gender)
prop.table(t)

# Create boxplot showing relationship between gender and SBP
boxplot(SBP~Gender,xlab="Gender",ylab="SBP") 
t.test(SBP~Gender) # t-test shows there is no significant difference
cor.test(SBP,Gender) # insignificant correlation

# (d) 
lin.blood.model.full <- lm(SBP~Age+Weight+Gender,data = blood.data) 
lin.blood.model.full <- lm(SBP~.,data = blood.data) # shorter code for previous line
summary(lin.blood.model.full)

# gender is insignificant!
# We might be able to improve the model by removing gender from the predictors

predict(lin.blood.model.full,data.frame(Gender=0,Age=80,Weight=190))
predict(lin.blood.model.full,data.frame(Gender=1,Age=80,Weight=190))

