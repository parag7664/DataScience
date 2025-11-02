# Regression Tutorial & Lab Exercise Sheet 1

# Q1 (h) 
# (i) Scatter plot
RetireAge<-c(57,62,60,57,65,60,58,62,56)
DeathAge<-c(71,70,66,70,69,67,69,63,70)

# A data frame can also be be used to store the above vectors.
dataQ1<-data.frame(RetireAge,DeathAge)
View(dataQ1)

# To access data in the data frame we can use the following
dataQ1$RetireAge
dataQ1$DeathAge

# If we do not want to use the $ notation we can use the 
# attach function

plot(RetireAge,DeathAge,xlab="Retirement Age (Years)",
     ylab="Age of Death (Years)") 

# (ii) Correlation coefficient
modelQ1<-lm(DeathAge~RetireAge) # create linear model
modelQ1

# For more info about model
summary(modelQ1)
coef(modelQ1) # see regression coefficients
modelQ1$coef # see regression coefficients

# (iii) Regression line plot
plot(RetireAge,DeathAge,xlab="Retirement Age (Years)",
     ylab="Age of Death (Years)")
abline(modelQ1)

# (iv) Correlation coefficient
cor(RetireAge,DeathAge) 

# (v) R-squared
cor(RetireAge,DeathAge)^2
summary(modelQ1) # We can also use the summary function to see R-squared

# Model Prediction
# We can use the predict function to calculate y values for given x
predict(modelQ1,data.frame(RetireAge = 76))  # one x values
xvalues<-c(60,61,62,63) # several x values
predict(modelQ1,data.frame(RetireAge = xvalues))  

# (vi)
mean(RetireAge)
median(RetireAge)

mean(DeathAge)
median(DeathAge)

# (vii) Histograms one above the other
par(mfrow=c(2,1))
hist(RetireAge,xlab="Retirement Age (Years)",main="Retirement Age Data")
hist(DeathAge,xlab="Age of Death (Years)",main="Age of Death Data")

# (viii) Boxplots side by side
par(mfrow=c(1,2))
boxplot(RetireAge,ylab="Retirement Age (Years)")
boxplot(DeathAge,ylab="Age of Death (Years)")

quantile(RetireAge)
quantile(DeathAge)

summary(RetireAge)
summary(DeathAge)

#######################################################################

# Q2 
# Read in the auto_sales.csv dataset.
# This contains several variables we will perfrom linear regerssion 
# between each of these variables individually


# (a) Input data
auto_data <- read.csv("auto_sales.csv")
auto_data
View(auto_data)
head(auto_data) # Look at first 6 rows of data. Useful for larger data sets. 

# (b) Summary of data
summary(auto_data)

# (c) Scatter plots
plot(auto_data$outlets,auto_data$sales,xlab="No. of Outlets",
     ylab="Sales")

# Eliminate the need for use of $
attach(auto_data)
par(mfrow=c(2,3))
plot(outlets,sales,xlab="No. of outlets",ylab="Sales")
plot(reg_auto,sales,xlab="Reg auto",ylab="Sales")
plot(Per_ncome,sales,xlab="Per ncome",ylab="Sales")
plot(Av_age_auto,sales,xlab="Av. Age",ylab="Sales")
plot(supervisors,sales,xlab="Supervisors",ylab="Sales")

# (d) 
modelQ2_outlet<-lm(sales~outlets)
modelQ2_reg_auto<-lm(sales~reg_auto)
modelQ2_Per_ncome<-lm(sales~Per_ncome)
modelQ2_Av_age_auto<-lm(sales~Av_age_auto)
modelQ2_supervisors<-lm(sales~supervisors)

# (e)
par(mfrow=c(2,3))
plot(outlets,sales,xlab="Outlets", ylab="Sales")
abline(modelQ2_outlet)

plot(reg_auto,sales,xlab="Reg Auto", ylab="Sales")
abline(modelQ2_reg_auto)

plot(Per_ncome,sales, xlab="Per ncome", ylab="Sales")
abline(modelQ2_Per_ncome)

plot(Av_age_auto,sales,xlab="Av Age Auto", ylab="Sales")
abline(modelQ2_Av_age_auto)

plot(supervisors,sales,xlab="Supervisors", ylab="Sales")
abline(modelQ2_supervisors)

# (f) 
summary(modelQ2_outlet)
summary(modelQ2_reg_auto)
summary(modelQ2_Per_ncome)
summary(modelQ2_Av_age_auto)
summary(modelQ2_supervisors)

# (g)
# Create model with all predictors included - multiple linear regression!
auto.model.all <- lm(sales~., data = auto_data)

# Investigate model parameters. Note which terms are significant and which are not!!
summary(auto.model.all) 

# Q3(e) - Similar to Q1(h)
