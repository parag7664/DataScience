df <- read_tsv("D:/Sem 2/statistical Data analysis/lab/soil_data.txt")
df <- pivot_longer(df, cols = c("Sand", "Clay", "Loam"), names_to = c("soil"), values_to = c("Yield"))

df %>% group_by(soil) %>% 
  summarise(mean_yield = mean(Yield), sd_yield = sd(Yield))

#To carry out a one way ANOVA, we need to ensure that the Soil variable is defined to be a factor, we can check:
is.factor(df$soil)#false

df$soil<-as.factor(df$soil)
levels(df$soil) #names of factor levels
nlevels(df$soil) #number of levels

#To visualise the effect of soil type on yield we can use a box and whisker plot of Yield vs. Soil
boxplot(Yield ~ soil, data=df)

#We can test the potential outlier using grubbs.test()
install.packages("outliers")
library(outliers)
grubbs.test(df$Yield[df$soil == "Clay"])

# Before proceeding with the ANOVA we will check 
#that homogeneity of variance is satisfied. This can be done using Fisher's F Test (to compare two variances)

#Let's calculate the variance for the different soil types:
df %>% group_by(soil) %>% 
  summarise( var_yield = var(Yield))

#To test whether the variances are the same for the different treatments we test the null 
#hypothesis:
#H0 = the variances crop yield for the different soil types are the same. 
#HA = the variances of crop yield for the different soil types are not the same.

fligner.test(Yield~ soil, data = df)

#Here the p-value is 0.8332 so we fail to reject the null hypothesis. There is not enough 
#evidence to conclude that variances are significantly different from one another and we 
#may apply the ANOVA. 

#In R the one way ANOVA model is specified by:
#aov(y~x)
#where y is the response variable and x is the explanatory variable.

model_treat<- aov(df$Yield~df$soil) 
summary(model_treat)

#n the last column, instead of a critical F Value we have a p-value which 
#tells us that the probability of obtaining the F value of 4.245 or more if the null 
#hypothesis is true is 0.025 or a 1 in 40 chance. The asterisk indicates that the difference 
#between the soil means is significant at the 5% level but not at the 1% level.

summary.lm(model_treat)

# The estimate for the intercept is the average yield for the Clay treatment. 
# The estimate for Loam gives the difference in mean yield between the Clay treatment 
# and the Loam treatment.
# The estimate for Sand gives he difference in mean yield between the Clay treatment 
# and the Loam treatment.
# We can write down the treatment effects model as: 
#   ???????????? = ????0 + ????1????1 + ????2????2
# ???????????? = 11.5 + 2.8????1 ??? 1.6????2
# Where for the Clay treatment  ????1 = 0 and  ????2 = 0. 
# For the Loam treatment and ????1 = 1 and  ????2 = 0. 
# For the Sand treatment ????1 = 0 and ????2 = 1.
# Note that the p-values reported show that mean yield for Loam and Sand was not 
# significantly different to the mean yield for Clay at the 5% significance level.

# We can use the relevel() command to set Sand to be the baseline.
df$Soil <- relevel(soil, "Sand")
model_treat<- aov(df$Yield~df$soil) 
summary(model_treat)
# We can view the treatment contrasts in matrix form:
contrasts(df$soil)

# We can see that the Loam column represents the ????1 dummy variable in the model above, 
# and the Sand column represents the ????2 dummy variable.
# To obtain the effects model where each treatment is compared to the overall mean, we 
# can specify sum contrasts:
contrasts(df$soil) <- contr.sum 
# Now we can rerun the ANOVA model again:
model_sum<- aov(df$Yield~df$soil) 
summary(model_sum)        
# Note that there is no change to the ANOVA table but the treatment estimates are 
# different:
summary.lm(model_sum) 

# Notice that for this output, the intercept coefficient corresponds to the overall mean. 
# The Soil1 coefficient corresponds to the difference between the overall mean and the 
# mean of the Clay treatment.
# The Soil2 coefficient corresponds to the difference between the overall mean and the 
# mean of the Loam treatment.
# What about the Sand treatment? To figure out how to calculate the mean of the Sand 
# treatment from the output, we need to look at the contrasts:
contrasts(df$soil)

# We can write down the model as: ???????????? = ???? + ????1????1 + ????2????2 + ????????????
# ???????????? = 11.9 ??? 0.4????1 + 2.4????2 + ????????????
# Where for the Clay treatment  ????1 = 1 and ????2 = 0 
# For the Loam treatment  ????1 = 0 and ????2 = 1 
# For the Sand treatment  ????1 = ???1 and ????2 = ???1

#the treatment effects graphically using  plot.design
plot.design(df$Yield~df$soil)

#To see the treatment effects in tabular form we can use the command model.tables:
  model.tables(model_treat)

  #To return the contrasts to the default setting with Clay as the baseline:
    contrasts(df$soil) <- NULL
  df$soil <- relevel(soil, "Clay")
  model_treat<- aov(df$Yield~df$soil) 
  summary(model_treat)
  
#   Effect Size ????2
#   To measure the effect size associated with a one way ANOVA, we use  ????2 (Eta squared).
#   ????2 = ????????????
#   ????????????
#   For the yield data set ????2 = 99.2
#   414.7 = 0.239 or 23%. We can say that 23.9% of the variation 
#   in yield was caused by soil type.
#   Pairwise Comparisons
#   For the yield example, the ANOVA found that there was a significant difference 
#   between the three soil types but did not tell us which types of soil had significantly 
#   different yields. To compare each pair of soil types we can use Tukey's honest significant 
# difference (HSD). 
  
TukeyHSD(model_treat)
plot(TukeyHSD(model_treat))

# We see that only the confidence interval for the difference in means between Sand and 
# Loam soil types does not contain 0. This tells us that the difference between treatment 
# means for Sand and Loam is significantly different to 0, i.e. the mean yield for Sand is 
# significantly different to the mean yield for Loam

# Model Validation
# Next we check the assumptions of the aov model (this is referred to as model 
#                                                 validation).
par(mfrow = c(2,2))
plot(model_treat)


# The plot(model_treat) command produces a series of commands, spread over 
# four pages (here compressed to a single page using the par(mfrow = c(2,2)) 
#             command). 
# The first graph (top left) shows a plot of the residuals against the fitted values.
# The residuals should be random with a mean of 0 and constant variation. 
# We see three sets of residuals plotted at each of the treatment means (the fitted values) 
# and there appears to be no pattern in the variance of the residuals (i.e. the variance is not 
#                                                                      increasing or decreasing with the mean).
# The second graph (top right) is the Normal Q-Q plot (quantile to quantile plot) which 
# plots the probability distribution of the standardised residuals with the standard normal 
# probability distribution. If the distribution of the residuals was a perfect standard normal 
# distribution then the Normal Q-Q plot would be a perfect straight line. Departures from 
# 10 11 12 13 14
# -10
# -5
# 0
# 5
# Fitted values
# Residuals
# Residuals vs Fitted
# 13
# 6
# 11
# -2 -1 0 1 2
# -2
# -1
# 0
# 1
# 2
# Theoretical Quantiles
# Standardized residuals
# Normal Q-Q
# 13
# 6
# 11
# 10 11 12 13 14
# 0.0
# 0.5
# 1.0
# 1.5
# Fitted values
# Standardizedresiduals
# Scale-Location
# 13
# 6
# 11
# -3
# -2
# -1
# 0
# 1
# 2
# Factor Level Combinations
# Standardized residuals
# Sand Clay Loamframe$soil :
#   Constant Leverage:
#   Residuals vs Factor Levels
# 13
# 6
# 11
# the straight line indicate non-normality. Points 6, 11 and 13 lie a little off the straight line 
# but this is nothing to worry about.
# The third graph (bottom left) is the Scale - Location plot which shows a plot of the 
# square root of the positive standardised residuals against the fitted values.  Like the 
# Residuals vs. Fitted plot it highlights any patterns the variance, each residual has been 
# standardised by dividing through by its estimated variance. When fitting a regression 
# model, the variance of the residuals can be smaller at the ends of the regression line. 
# standardised residuals account for this and can be used to detect outliers and 
# hetroscedasticty. 
# The fourth graph shows Cook's distance which measures how influential a point is in the 
# analysis. Cook's distance measures the effect of deleting a given observation on the 
# statistical model. Data points with large residuals (outliers) and/or high leverage may 
# often have a large Cook's distance value and are considered to merit closer examination 
# in the analysis.
# Leverage points are those observations made at extreme or outlying values of the 
# independent variables such that the lack of neighboring observations means that the fitted 
# regression model will pass close to that particular observation.
# In our plot there seem to be no influential points that might be having a large effect on the 
# parameter estimates.
# Reporting the Results
# We can report the results of the ANOVA as follows:
#   A one way analysis of variance indicated that the type of soil the crop was grown in 
# significantly affected the crop yield, F(2, 27) = 4.245, MSE = 11.69, p = .025, ????2
# =0.239. As shown in Table 1, Tukey's HSD test indicated that the mean yield of crops 
# grown on loam soil was significantly greater than the mean yield of crops grown on sand 
# soil. 


