# for the Wage dataset
install.packages("ggstatsplot")
library(ggstatsplot)        # publication ready visualizations with statistical details
install.packages("flextable")
library(flextable)          # beautifying tables
install.packages("summarytools")
library(summarytools)       # EDA
install.packages("psych")
library(psych)              # psychological research: descr. stats, FA, PCA etc.
install.packages("skimr")
library(skimr)              # summary stats
install.packages("gtsummary")
library(gtsummary)          # publication ready summary tables
install.packages("moments")
library(moments)            # skewness, kurtosis and related tests
install.packages("ggpubr")
library(ggpubr)             # publication ready data visualization in R
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) # econometrics for performance and risk analysis
install.packages("fastStat")
library(fastStat)           # well :) you've guessed it
install.packages("performance")
library(performance)        # Assessment of Regression Models Performance (for outliers here)

A <- c(44    , 9    , 10  ,   13,     18,     3 ,    29,    11 ,   12  ,  38)
summary(A)
control <- c(11, 13, 9, 4, 34, 17, 18, 14, 12,13, 26, 31)
drug <- c(34,31,35, 29, 28, 4, 6, 30, 5, 22, 8, 21)

boxplot(control, drug)
hist(control)
hist(drug)

shapiro.test(control)#W = 0.90786, p-value = 0.2003 normally distributed
shapiro.test(drug)#W = 0.85136, p-value = 0.03816

library(e1071)

skewness(drug)
skewness(control)

wilcox.test(control, drug, alternative = "less") #W = 58.5, p-value = 0.22

#is the median value diffrent for both , --- there is no significance difference in median

#questiin 2
right <- c(50,45,33,22,99,79,4,36,62,51,27,15,26,83,86)
left <- c(47,45,31,24,78,76,13,46,45,44,23,14,34,79,81)

boxplot(right, left)
hist(right)
hist(left)

# both are normally distributed
shapiro.test(right)
shapiro.test(left)

diff = left-right
shapiro.test(diff) #W = 0.91749, p-value = 0.1764

t.test(diff, mu=0)
t.test(left, right,paired = T)

wilcox.test(left, right, paired = T)
wilcox.test(diff)

install.packages("BSDA")
library(BSDA)
SIGN.test(diff) #does not have assumption of symmetry - symmetry is violated


#Question 3
#Using pet.csv investigate whether there is a difference between the GPA for those who have 
#a pet and those that don't and also whether there is a significant difference in grades between Year 
