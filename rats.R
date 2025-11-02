#one sample t-test
time <- c(56.7, 72.1, 49.0, 62.1, 80.0, 77.1, 58.4, 55.5,68.4, 58.8, 59.3, 57.1, 61.2, 26.1, 34.7, 65.7)

shapiro.test(time)#W = 0.92065, p-value = 0.1728
#we fail to reject H0 so it is normally distributed

#we wont calculate var
# this is 2 sided test because we checking wheter mean = 60 or diffrent in both directions

# h0 - mean is = 60
# ha - mean is !=60
?t.test
t.test(time, mu=60)
#if t value is greated than 3/4 or more than the p value is very low vice-versa
#t = -0.32103, df = 15, p-value = 0.7526

#p value is >0.05 we reject the null hypothesis
# alternative hypothesis: true mean is not equal to 60
# 95 percent confidence interval:
#   51.50105 66.27395
#this contains 60 means that sample could have come from population with a mean of 60
# sample estimates:
#   mean of x 
# 58.8875 # this is sample mean

# t value is -0.32, means that the sample we got is -0.32 SD away from the population
#we fail to reject null hypothesis, the ample mean is is not significantly diffrent than the true population mean of 60


#question 2
#independent sample t-test 
# do shapiro, independence test, constant variance 
#if car is violated we will do var.equal=F
#H0 - mu women >= mu of man
#Ha - mu women < mu of man 

Men <- c(77.8, 71.9, 75.8, 75.6, 71.8, 73.8, 66.9, 77.6, 74.1, 72.0)
Women <- c(67.4, 85.4, 76.6, 59.8, 68.2, 65.1, 67.1, 70.8, 78.1, 79.5)

shapiro.test(Men)
shapiro.test(Women)
#normally distributes

var.test(Men, Women)# var = false in this case
#The p-value of F-test is p-value = 0.01584 which is greater than the significance level 0.05. In conclusion, there is no significant difference between the two variances.

# h0 - women have a significantly < resting pulse than men
# ha - women have a significantly >= resting pulse than men  

  
t.test(Men, Women, conf.level = 0.99, var.equal = T)
#p>0.05 p-value = 0.481 fail to reject null hypothesis




#The p-value of F-test is p = 0.2331433 which is greater than the significance level 0.05. In conclusion, there is no significant difference between the two variances.