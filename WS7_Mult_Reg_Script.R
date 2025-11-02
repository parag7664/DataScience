library(tidyverse)
#install.packages("faraway")
library(faraway)
library(car)
library(summarytools)

########### Load data into working memory ###########
savings<- savings
savings<- as_tibble(savings)

########### Examine the data ###########
## Pairs plot
##function to put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


## function to put correlations on the upper panels,
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 2
  text(0.5, 0.5, txt, cex = cex.cor)
}
## pairs plot 
windows(10,10)
pairs(savings, diag.panel = panel.hist, upper.panel = panel.cor)

dfSummary(savings)

## It can be helpful to examine simple linear models for each predictor ##
m_pop15<-lm(sr ~ pop15, data = savings)
m_pop75<-lm(sr ~ pop75, data = savings)
m_dpi<-lm(sr ~ dpi, data = savings)
m_ddpi<-lm(sr ~ ddpi, data = savings)

## plot each of the regression lines
windows(10,10)
par(mfrow = c(2,2)) 
plot(sr~pop15, data = savings)
abline(m_pop15)
plot(sr~pop75, data = savings)
abline(m_pop75)
plot(sr~dpi, data = savings)
abline(m_dpi)
plot(sr~ddpi, data = savings)
abline(m_ddpi)

##############################################
# Explore added variable plots to understand how
# the beta coefficients are calculated  
################################################

## fit a model using two  predictors ##
# Note the coefficients associated with pop15 and ddpi

m1<-lm(sr ~ pop15 +  ddpi, data = savings)
summary(m1)

# create added variable plot for the regression model of 
# sr on pop15 adjusted for ddpi.

# First create the model pop15 ~ ddpi, the residuals of this model
# contain thee variation in pop15 that is not explained by ddpi
m_ddpi_pop15 <-lm(pop15 ~ ddpi, data = savings)

# plot residuals
# Note that the residuals of the model m_ddp contain the vriation in sr
# that is not explained by ddpi
plot(m_ddpi$res ~ m_ddpi_pop15$res, xlab = "residuals pop15 ~ ddpi", 
     ylab = "residuals sr ~ ddpi")

# fit a regression model to the residuals
# Note that the slope of the regression line is the estimate for the beta 
# coefficient associated with pop15 in the model containing both pop15 and ddpi
m_res <-lm(m_ddpi$res ~ m_ddpi_pop15$res)
abline(m_res)
summary(m_res)


###### Exercise #######
# Create a partial regression plot showing the relationship between 
# ddpi and sr adjusted for pop15. 

# first create the model pop15 ~ ddpi
m_pop15_ddpi <-lm(ddpi ~ pop15, data = savings)

# plot residuals
plot(m_pop15$res ~ m_pop15_ddpi$res, xlab = "residuals ddpi ~ pop15", 
     ylab = "residuals sr ~ pop15")

# fit a regression model to the residuals
# Note that the slope of the regression line is the estimate for the beta 
# coefficient associated with ddpi in the model containing both pop15 and ddpi
m_res <-lm(m_pop15$res ~ m_pop15_ddpi$res)
abline(m_res)
summary(m_res)

################################################
## Hypothesis tests##
################################################

## fit the full model using all four predictors ##
m2<-lm(sr ~ pop15+pop75 +dpi + ddpi, data = savings)
summary(m2)

## Write down the fitted model 
#  For each coefficient, write down a sentence interpreting the meaning 
#  of the coefficient.
## sr = 28.5660865 -0.4611931xpop15 -1.6914977xpop75 -0.000336xdpi 
#       + 0.4096949xddpi
# the expected change in sr per unit change in pop15 when all the 
# remaining independent variables are held constant is -0.4611931.
# the expected change in sr per unit change in pop75 when all the 
# remaining independent variables are held constant is -1.6914977.
# the expected change in sr per unit change in dpi when all the 
# remaining independent variables are held constant is -0.0003369.
# the expected change in sr per unit change in ddpi when all the 
# remaining independent variables are held constant is 0.4096949.

########################################################
# Test the hypothesis:
# H0: β_1=β_2=β_3=β_4=0
# HA: at least one of the β_i≠0
# What do the results of the hypothesis test imply for the regression model?

# Examining the output for summary(m2) we see that the global F-statistic 
# is F(4, 45) = 5.756 and p =0.0007904, this F-statistic compares the fitted
# model to the null model (also called the intercept only model). In this instance 
# may reject the null hypothesis at the 1% confidence level and conclude that
# at least one of the predictors is associated with sr.

# Test the hypothesis:
# H0: β_3=0
# HA: β_3≠0
# What do the results of the hypothesis test imply for the regression model?

# Examining the output for summary(m2) we see that the t-statistic associated
# with β_3 (dpi) is -0.362 and the associated p-value is 0.719173
# In this instance we fail to reject the null hypothesis at the 5% confidence 
# level and conclude that dpi is not associated with sr (in the presence of the 
# other predictors).

# Next, let’s test the hypothesis: H0: β_2 = β_3=0
# HA: β_2,β_3 not both equal to 0

m3<-lm(sr~ pop15 + ddpi, data = savings)
summary(m3)

anova(m3,m2)

# Examining the output for anova(m3,m2) we see that the F-statistic 
# is F(2, 45) = 1,7233 and p =0.19, this F-statistic compares the fit of m3 to m2 
# In this instance we fail to reject the null hypothesis at the 5% confidence
# level and conclude that the variables pop75 and dpi are not associated with sr 
# (when pop15 and ddpi are included in the model)


##### variance inflation factors (VIFs) #########
vif(m2)

#### Diagnostic plots ######
#################################

#calculate standardised residuals
stdres <- rstandard(m3)

# plot standardised residuals against fitted values 
# and each explanatory variable
windows(18,8)
par(mfrow = c(1,3))
plot(stdres ~ m3$fitted)
plot(stdres ~ savings$pop15)
plot(stdres ~ savings$ddpi)

## Check normality
windows(5,5)
hist(stdres)
windows(10,10)
par(mfrow=c(2,2))
plot(m3) 



## calculate leverage
h<- lm.influence(m3)$hat
plot(h, xlab = "Observation", ylab = "Leverage")
abline(h=0.12)
identify(h,n=2)
windows(5,5)
##check regression model excluding Libya
m4 <-lm(sr ~ pop15 + ddpi, data = savings[-49,])
summary(m4)
summary(m3)

## check changes to coefficients excluding each observation in turn
changes<-lm.influence(m3)$coefficients
## plot changes to coefficients
windows(5,5)
plot(changes[,2], ylab = "changes to the coefficient for pop15")
plot(changes[,3], ylab = "changes to the coefficient for ddpi")

## check changes to coefficients excluding each observation in turn
changes<-lm.influence(m4)$coefficients
## plot changes to coefficients
windows(5,5)
plot(changes[,2], ylab = "changes to the coefficient for pop15")
plot(changes[,3], ylab = "changes to the coefficient for ddpi")


##############################################
############### Exercise #####################
##############################################
gamb<-teengamb
# Sex is a categorical variable and we will not be using it as a predictor 
# in this exercise (if you are interested you could return to this data set 
# and include the variable sex after completing the materials on ANCOVA)

gamb[,1]<-NULL
########### 1 Examine the data ###########

summary(gamb)

## Pairs plot
##function to put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


## function to put correlations on the upper panels,
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 2
  text(0.5, 0.5, txt, cex = cex.cor)
}
## pairs plot ##
# Note that the gamble variable and income variables are skewed
# There is a positive correlation between gambling and income (r =0.62 )
# There is a positive correlation between status and verbal (r =0.53)
windows(10,10)
pairs(gamb, diag.panel = panel.hist, upper.panel = panel.cor)


##  boxplots ##
# There is one potential outlier in income (this may be down to skew)
# There are two potential outliers in verbal
# There are a number of outliers in gamble (this may be down to skew)
windows(10,10)
par(mfrow=c(2,2))
boxplot(gamb$status, xlab="status")
boxplot(gamb$income, xlab="income")
boxplot(gamb$verbal, xlab="verbal")
boxplot(gamb$gamble, xlab="gamble")


## It can be helpful to examine simple linear models for each predictor ##
m_status<-lm(gamble ~ status, data = gamb)
m_income<-lm(gamble ~ income, data = gamb)
m_verbal<-lm(gamble ~ verbal, data = gamb)

## plot each of the regression lines
# Note the relationship between gamb and each of the predictors
windows(10,7)
par(mfrow = c(1,3)) 
plot(gamble~status, data = gamb)
abline(m_status)
plot(gamble~income, data = gamb)
abline(m_income)
plot(gamble~verbal, data = gamb)
abline(m_verbal)

#### 2	Fit the model : y=β_0+β_1status + β_2income + β_3verbal+e ###
m1<-lm(gamble~status + income + verbal, data = gamb)
summary(m1)

#### 3. Intepret the coefficient for income
# An increase in income of £1 per week increases the annual expenditure on
# gambling by £5.7707 adjusting for the variables status and verbal.

#### 4.	Calculate the variance inflation factors for this model and discuss 
# their implications for collinearity in the model.
# The VIFs lie between 1 and 2 indicating that collinearity is not 
# having a large impact on the coefficient estimates for this model.
vif(m1)

### 5. Test the hypothesis:
# H0: β_1=β_2=β_3=0
# HA: at least one of the β_i≠0
# What do the results of the hypothesis test imply for the regression model?

# Examining the output for summary(m1) we see that the global F-statistic 
# is F(3, 43) = 11.49 and p =000012, this F-statistic compares the fitted
# model to the null model (also called the intercept only model). In this instance 
# may reject the null hypothesis at the 1% confidence level and conclude that
# at least one of the predictors is associated with gamble.

### 6. Test the hypothesis:
# H0: β_3=0
# HA: β_3≠0
# What do the results of the hypothesis test imply for the regression model?

# Examining the output for summary(m1) we see that the t-statistic associated
# with β_3 (verbal) is -1.809 and the associated p-value is 0.0775
# In this instance we fail to reject the null hypothesis at the 5% confidence 
# level and conclude that verbal is not strongly associated with gamble (in the
# presence of the  other predictors). Note however that the coefficient estimate
# is -4.1211 and 
# the p-value is relatively low, at the 10% confidence level, we would conclude 
# verbal is associated with gambling.  


### 7.	Assess the fit of the model using diagnostic plots and try to improve the 
### fit of the model.
windows(10,10)
par(mfrow=c(2,2))
plot(m1)

# The plot of residuals vs. fitted shows that the residuals to not seem to
# be random, many of the residuals are clustered together at the lower  
# fitted values - we should probably address the skew we observed when examining
# the histograms and scatterplots.

# try a log transformation - note that since the gamble variable
# contains zeros we need to add a small constant to the variable
# gamble before transforming

gamb$log_gamble<-log(gamb$gamble+0.1)
windows(10,10)
pairs(gamb, diag.panel = panel.hist, upper.panel = panel.cor)

# refit model
m2<-lm(log_gamble~status + income + verbal, data = gamb)
summary(m2)
windows(10,10)
par(mfrow=c(2,2))
plot(m2)
# The pattern in the plot of residuals vs fitted appears to be gone however
# we note some observations with high leverage
# Try transforming the income variable

gamb$log_income<-log(gamb$income)
windows(10,10)
pairs(gamb, diag.panel = panel.hist, upper.panel = panel.cor)

# refit model
m3<-lm(log_gamble~status + log_income + verbal, data = gamb)
summary(m3)
windows(10,10)
par(mfrow=c(2,2))
plot(m3)

# The diagnostic plots have improves, there is one observation that has high leverage 
# which could be investigated.

