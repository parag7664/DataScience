#Paired
#question 1

Before <-	c(185,140,143,164,176,136,148,139,166,153,149,131,166,170,125)
After <- c(139,136,140,125,143,129,138,123,129,128,126,132,132,142,136)
boxplot(After, Before)
diff <- After-Before
summary(diff)

# null hypothesis = diff is equal to 0
# Alternative = diff is less than 0 here is a significant decrease in the mean blood pressure 
#recorded after the treatment compared to that recorded before the treatment? 

shapiro.test(diff)
# p-value = 0.7392 i.e data is normaly distributed. 

t.test(diff, mu=0, alternative="less", conf.level=0.95)
t.test(After, Before, paired=T, alternative="less", conf.level=0.95)

#Paired t-test

#data:  After and Before
#t = -4.4658, df = 14, p-value = 0.0002665
#alternative hypothesis: true difference in means is less than 0
#95 percent confidence interval:
#  -Inf -11.82933
#sample estimates:
#  mean of the differences 
#-19.53333 

#since p-value is <0.05 we reject the null hypothesis so,
# there is significant decrease in the mean blood pressure recorded 
#after the treatment compared to that recorded before the treatment.

#question 2
#investigate whether mobile phone use impairs reaction time while driving
# reaction time in milliseconds were recorded for 10 people

#independent sample
reaction_time_phone_use <- c(636.31, 646.78, 574.92, 446.71, 552.36, 441.96, 529.97, 543.63, 720.98, 530.47)
reaction_time_nonUser <- c(465.63, 468.52, 465.17, 496.52, 488.32, 491.41, 529.19, 416.04, 511.44, 657.77)

driving <- c(636.31, 646.78, 574.92, 446.71, 552.36, 441.96, 529.97, 543.63, 720.98, 530.47,465.63, 468.52, 465.17, 496.52, 488.32, 491.41, 529.19, 416.04, 511.44, 657.77)
usage <- c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2)

#Analyse whether the population mean reaction time would differ for the two groups. Use a level of significance of 0.01

boxplot(reaction_time_phone_use, reaction_time_nonUser)
#reaction time of phone user is more than non users from the boxplot.

summary(reaction_time_phone_use)
summary(reaction_time_nonUser)

#test for normality
#if one of them of is normal consider it as normal
#H0 is its normal
shapiro.test(reaction_time_phone_use) # W = 0.94828, p-value = 0.6482
shapiro.test(reaction_time_nonUser) # W = 0.82069, p-value = 0.02583

#test for variance
#H0 valiance is eual
var.test(reaction_time_phone_use, reaction_time_nonUser, conf.level = 0.99)
#p value >0.05 so variance is euqal 
#bartlett.test(driving~usage)

#H0 =  population mean reaction time would differ for the two groups.
# h1 = mu1 = mu2
t.test(reaction_time_phone_use, reaction_time_nonUser, conf.level = 0.99, var.equal = T)
t.test(driving~usage, conf.level = 0.99, var.equal = T)
#p value > 0.05 so population mean differs, 7% among the sample have difference in mean

# The p-value of 0.07243 tells us that if the mean filling volume of the machine were 500 ml, the 
# probability of selecting a sample with a mean volume less than or equal to this one would be 
# approximately 7%.
