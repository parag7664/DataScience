setwd("D:/Sem 2/statistical Data analysis/assignment 2")
porosityDF = read.csv("porosityF.csv")
str(porosityDF)


#question a
attach(porosityDF)
View(porosityDF)

summary(porosityDF)
head(porosityDF)
tail(porosityDF)

windows(10,10)
par(mfrow = c(3,3))
boxplot(density)
boxplot(residue)
boxplot(grain)
boxplot(calcite)
boxplot(dolmite)
boxplot(porosity)


cor(porosityDF)

pairs(porosityDF)

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
pairs(porosityDF, diag.panel = panel.hist, upper.panel = panel.cor)


library(PerformanceAnalytics)
windows(10,10)
chart.Correlation(porosityDF)

########################3
# b

density_model<-lm(porosity ~ density, data = porosityDF)
residue_model<-lm(porosity ~ residue, data = porosityDF)
grain_model<-lm(porosity ~ grain, data = porosityDF)
calcite_model<-lm(porosity ~ calcite, data = porosityDF)
dolmite_model<-lm(porosity ~ dolmite, data = porosityDF)

## plot each of the regression lines
windows(10,10)
par(mfrow = c(3,3)) 
plot(porosity ~ density, data = porosityDF)
abline(m_pop15)
plot(porosity ~ residue, data = porosityDF)
abline(m_pop75)
plot(porosity ~ grain, data = porosityDF)
abline(m_dpi)
plot(porosity ~ calcite, data = porosityDF)
abline(m_ddpi)
plot(porosity ~ dolmite, data = porosityDF)
abline(m_dolmite)


model <- lm(porosity ~ density + residue + grain + calcite + dolmite, data = porosityDF)
summary(model)


library(car)
#calculate the VIF for each predictor variable in the model
vif(model)


#create vector of VIF values
vif_values <- vif(model)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)


#b iii)

# Create a partial regression plot showing the relationship between 
# ddpi and sr adjusted for pop15. 

# first create the model pop15 ~ ddpi
model1 <-lm(density ~ residue + grain + calcite + dolmite, data = porosityDF)
model2 <- lm(porosity ~ residue + grain + calcite + dolmite, data = porosityDF)

# plot residuals
plot(model2$res ~ model1$res, xlab = "residuals residue", 
     ylab = "residuals porosity ~ residue")

# fit a regression model to the residuals
# Note that the slope of the regression line is the estimate for the beta 
# coefficient associated with ddpi in the model containing both pop15 and ddpi
m_res <-lm(model2$res ~ model1$res)
abline(m_res)
summary(m_res)

#test hypothesis

## fit the full model using all four predictors ##
m2<-lm(porosity ~ density + residue + grain + calcite + dolmite, data = porosityDF)
summary(m2)


windows(10,10)
par(mfrow=c(2,2))
plot(m2)


## v)

#calculate standardised residuals
stdres <- rstandard(m2)

# plot standardised residuals against fitted values 
# and each explanatory variable
windows(10,5)
par(mfrow = c(3,3))
plot(stdres ~ m2$fitted)
plot(stdres ~ porosityDF$density)
plot(stdres ~ porosityDF$residue)
plot(stdres ~ porosityDF$grain)
plot(stdres ~ porosityDF$calcite)
plot(stdres ~ porosityDF$dolmite)

## Check normality
windows(5,5)
hist(stdres)
windows(10,10)
par(mfrow=c(2,2))
plot(m2) 

## calculate leverage
h<- lm.influence(m2)$hat
plot(h, xlab = "Observation", ylab = "Leverage")
abline(h=0.29)

##check regression model excluding Libya
m4 <-lm(sr ~ pop15 + ddpi, data = savings[-49,])
summary(m4)
summary(m3)

##not required no influencial points
## check changes to coefficients excluding each observation in turn
changes<-lm.influence(m2)$coefficients
changes


## plot changes to coefficients
windows(15,15)
par(mfrow=c(3,3))
plot(changes[,2], ylab = "changes to the coefficient for density")
plot(changes[,3], ylab = "changes to the coefficient for residue")
plot(changes[,4], ylab = "changes to the coefficient for grain")
plot(changes[,5], ylab = "changes to the coefficient for calcite")
plot(changes[,6], ylab = "changes to the coefficient for dolmite")

## check changes to coefficients excluding each observation in turn
changes<-lm.influence(m4)$coefficients
## plot changes to coefficients
windows(5,5)
plot(changes[,2], ylab = "changes to the coefficient for pop15")
plot(changes[,3], ylab = "changes to the coefficient for ddpi")

#question c)
#when density = 2.7, residue = 4.43, grain_length = 11.8 calcite = 60 and dolmite = 28.7

#Creating a data frame
new_variable<-data.frame(density = 2.7, residue = 4.43, grain = 11.8, calcite = 60, dolmite = 28.7)

#predicts the future values
predict(m2,newdata = new_variable)
predict(m2,newdata = new_variable,interval = 'confidence')
#1.529041


#question d
# (d)	Compare the full model to the model where calcite and dolmite are excluded using 50 repeats of 10-fold cross validation

#excluded_model <- lm(porosity ~ density + residue + grain , data = porosityDF)

library(caret)

#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 10, repeats = 50)

#fit a regression model and use k-fold CV to evaluate performance
model <- train(porosity ~ density + residue + grain , data = porosityDF, method = "lm", trControl = ctrl)

#view summary of k-fold CV               
print(model)
summary(m2)


