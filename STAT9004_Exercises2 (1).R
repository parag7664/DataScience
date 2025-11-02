#########################################################
########        Exercise Sheet 2           #########
########                                   #########

########################################################


#######  Q.5  The Central Limit Theorem in Action 
####################################################

n_samples <- 10000   ### the number of samples we take
n <-1                ### the size of each sample


## create a vector containing the results of one roll of the die 
## repeated 10,000 times. This shows the distribution of the random variable X
x1 <- as.integer(runif(n_samples,1,7) )    
x1
## plot the results in histogram
windows(15,20)
par(mfrow=c(3,2))
hist(x1,main=paste( " Disribution of the mean of", n, "roll"),breaks=seq(0.5,6.5, 1), freq = FALSE)  



## create a vector containing the results of the average of two rolls of the 
## die repeated 10,000 times. This shows the distribution of the sample mean when the 
## sample is of size 2

n <-2 

sample <-rep(0,n_samples)        #create a vector of zeros of length n_samples

for (k  in 0:n_samples)            
{ sample[k] <- mean(as.integer(runif(n,1,7))) 
}

hist(sample,main=paste( " Disribution of the mean of", n, "rolls"),breaks=seq(0.5,6.5, 0.5), freq = FALSE)  


## create a vector containing the results of the average of two rolls of the 
## die repeated 10,000 times. This shows the distribution of the sample mean when the 
## sample is of size 5

n <-5 

sample=rep(0,n_samples)        #create a vector of zeros of length n_samples

for (k  in 0:n_samples)            
{ sample[k] <- mean(as.integer(runif(n,1,7))) 
}

hist(sample,main=paste( " Disribution of the mean of", n, "rolls"),breaks=seq(0.5,6.5, 0.2), freq = FALSE)  


## create a vector containing the results of the average of two rolls of the 
## die repeated 10,000 times. This shows the distribution of the sample mean when the 
## sample is of size 10

n <-10 

sample=rep(0,n_samples)        #create a vector of zeros of length n_samples

for (k  in 0:n_samples)            
{ sample[k] <- mean(as.integer(runif(n,1,7))) 
}

hist(sample,main=paste( " Disribution of the mean of", n, "rolls"),breaks=seq(0.5,6.5, 0.1), freq = FALSE)  



## create a vector containing the results of the average of two rolls of the 
## die repeated 10,000 times. This shows the distribution of the sample mean when the 
## sample is of size 100

n <-100 

sample=rep(0,n_samples)        #create a vector of zeros of length n_samples

for (k  in 0:n_samples)            
{ sample[k] <- mean(as.integer(runif(n,1,7))) 
}

hist(sample,main=paste( " Disribution of the mean of", n, "rolls"),breaks=seq(0.5,6.5, 0.06), freq = FALSE)  

## create a vector containing the results of the average of two rolls of the 
## die repeated 10,000 times. This shows the distribution of the sample mean when the 
## sample is of size 1000

n <-1000 

sample=rep(0,n_samples)        #create a vector of zeros of length n_samples

for (k  in 0:n_samples)            
{ sample[k] <- mean(as.integer(runif(n,1,7))) 
}

hist(sample,main=paste( " Disribution of the mean of", n, "rolls"),breaks=seq(0.5,6.5, 0.05), freq = FALSE)  

##### Create a Loop  #####

## create a vector containing the sample sizes we would like to plot
n <- c(1, 2,5,10,100,1000)
## create a matrix of zeros with 10000 rows and 6 columns
X <- matrix(0, n_samples, length(n))


windows(15,20)
par(mfrow=c(3,2))

for (i in n)
{ j = which(n == i)
  
  for (k  in 1:n_samples)  
  {
  X[k,j] <- mean(as.integer(runif(i,1,7))) 
  }
hist(X[,j],main=paste( " Disribution of the mean of", i, "rolls"), freq = FALSE, xlim = c(0.5,6.5))  
}

############ Power Analyses ##########

install.packages("pwr")
install.packages("asbio")
library(pwr)
library(asbio)

## Q.6 ##################################
### check the answer to part b.
power.z.test(sigma = 1.35, n = 50, alpha = 0.05, effect = 0.5, test = "two.tail", strict = FALSE)

pwer 

### part d.
power.z.test(sigma = 1.35, power = 0.9, alpha = 0.05, effect = 0.25, test = "two.tail", strict = FALSE)

## Q.7 ###################################

## Note that here we test the mean difference between two samples therefore
## a two sample t-test is appropriate. We calculate the power for a two sample t
## test in exactly the same way as for a one sample t-test except this time 
## the effect size is the difference between the two sample means.


# a.
# n = 20 (n must be the number in each group, 20 days at each site)
# sigma = 6.2 (pooled standard deviation)
# alpha = 0.05
# we want to detect a minimum difference of 5 complaints so d = (5/6.2)
pwr.t.test(n = 20, d = (5/6.2), sig.level=0.05, type = "two.sample", alternative = "two.sided")


# b.
pwr.t.test(n = 20, d = (5/6.2), sig.level=0.01, type = "two.sample", alternative = "two.sided")

# c.
pwr.t.test(power = 0.9, d = (5/6.2), sig.level=0.05, type = "two.sample", alternative = "two.sided")


######  Q8 ######
## a.	H0: mean level of HbA1c for paients with no intervention = mean level of HbA1c for paients with  intervention
##    HA: mean level with intervention < mean level with no intervention

## b.	What are the type I and type II errors associated with this hypothesis?
## Type I concluding that the intervention does reduce mean level of HbA1c when 
## it does not.
## Type II oncluding that the intervention does not reduce mean level of HbA1c when 
## it does.
 
## c.	Calculate the minimum number of samples required to test the hypothesis with 80% power. 
## You may assume a significance level of 5%.

pwr.t.test(power = 0.8, d = (1/2.2), sig.level=0.05, type = "one.sample", alternative = "less")
