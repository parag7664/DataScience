rm(list = ls()) # clears enviornment
help.start()
?plot

# exeercise 5,6,7.
a = seq(1,10,1)
b = seq(2,20,2)
plot(a,b,'s',main="First Plot",sub="Sub Title",xlab="A",ylab="B")

plot(sin, -pi, 2*pi)

# exercise 8
a=2
a=3
b <- 3
a*b
c=a*b
b/a
3*6+234
exp(log(10))
a=2; is.logical(a); is.list(a); is.numeric(a)

#10 - math problem
vol_in=1000
con_in=2
vol_out=997
con_out=1.95
yield = (vol_out*con_out)/(vol_in*con_in)
print("The yield is:") ; print(yield)

# exercise 11
data("mtcars")
mtcars

#exercise 12
plot.ts(data)
df <- data.frame()
library(ggplot2)
ggplot2::
ggplot(mtcars,aes(x=mpg, y=qsec))+ geom_point(color="red")+labs(title="data set",x="MPG",y="QSec")
ggplot(mtcars,aes(x = mpg, y = qsec)) + geom_point(color='blue') + labs(title="Title test", x="MPG", y= "Qsec")

#exercise 13

x = c(0.1, 2, 4.3, 3.1, 5)

x[2]# returns the second value in that vector x[1:3] #returns the first three values 
x[1:3]
x[-(1:3)]
x[x>3]
N = length(x)

# exercise 14
y = 2*x+1
y
mean(x)
median(x)
sd(x)

mean(y)
median(y)
sd(y)

seq(-5,5,1)
rep(y,2)

fruit <-c(5, 10, 4)
names(fruit) <-c("orange", "banana", "apple") 
lunch <-fruit[c("apple","orange")]
fruit

lunch

x = c(1, 2, 3, 4, 5, 6, 7, 8, 9) 
matrix(x, ncol = 3)
M = matrix(x, ncol = 3, byrow = TRUE)
dim(M)
ncol(M)
nrow(M)
t(M);aperm(M, c(2,1))
diag(M)
det(M)

A <-M; B <-t(M)
A * B

A %*% B

h = seq(1, 24, by=1)
Z <-array(h, dim=c(3,4,2))
Z[1,1,1]; Z[1,2,2]

Lst <-list(name="Fred", wife="Mary", no.children=3,child.ages=c(4,7,9))
Lst
Lst[[2]]
Lst[[4]][1]

Lst[4]
Lst[[4]]
Lst$child.ages

format(Sys.Date(), "%a %b %d")
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
lct
x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
z <- as.Date(x, "%d%b%Y")
z
## date given as number of days since 1900-01-01 (a date in 1989)
as.Date(37637, origin = "1900-01-01")

dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(dates, "%m/%d/%y")

## Excel is said to use 1900-01-01 as day 1 (Windows default) or
## 1904-01-01 as day 0 (Mac default), but this is complicated by Excel
## incorrectly treating 1900 as a leap year.
## So for dates (post-1901) from Windows Excel
as.Date(35981, origin = "1899-12-30") # 1998-07-05
## and Mac Excel
as.Date(34519, origin = "1904-01-01") # 1998-07-05

## Experiment shows that Matlab's origin is 719529 days before ours,
## (it takes the non-existent 0000-01-01 as day 1)
## so Matlab day 734373 can be imported as
as.Date(734373, origin = "1970-01-01") - 719529 # 2010-08-23       

## Time zone effect
z <- ISOdate(2010, 04, 13, c(0,12)) # midnight and midday UTC
as.Date(z) # in UTC

## these time zone names are common
as.Date(z, tz = "NZ")
as.Date(z, tz = "HST") # Hawaii

# Exeercise 20
a <- seq(1,5,1)
emp = data.frame(emp_id = seq(1,5,1),
           emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
           salary = c(623.50, 515.20,456.39, 874.45, 493.10),
           start_date = c(as.Date("6/30/2016","5/30/2016","4/30/2016","3/30/2016","2/30/2016", format = "%m/%d/%Y")))
emp
# Exercise 21
# Get the structure of the data frame.
str(emp)
#note the different types of variables
# Print the summary.
summary(emp)

# Exercise 22
dept = c("IT","sales","IT","HR","Finance")
emp_data_rep <- emp
emp_data_rep$dept <- dept
emp_data_rep

emp_data_rep<- emp                       # Replicate example data
emp_data_rep["dept"] <- dept             # Add new column to data\

data_3 <- data                           # Replicate example data
data_3 <- cbind(data, new_col = vec)     # Add new column to data

emp$dept <- dept
emp


# Exercise 23

emp.newdata = data.frame(emp_id = seq(6,8,1),
                 emp_name = c("Parag","Pranav","amit"),
                 salary = c(620.50, 315.20,416.39),
                 start_date = c(as.Date("1/20/2016","4/10/2016","8/10/2016", format = "%m/%d/%Y")),
                 dept = c("IT","Operations","Finance"))
emp.newdata

# Exercise 24 Combine the two dataframes

final_emp_data = rbind(emp, emp.newdata)
final_emp_data

##
##Try the plyr package:
##rbind.fill(a,b,c),
##bind_rows(a,b)
# from the dplyr library ##

# Exercise 25
final_emp_data[nrow(final_emp_data) + 1,] = c("9","john","763.3","1/1/2021","IT")
final_emp_data


###There's now add_row() from the tibble or tidyverse packages.
###library(tidyverse)
###df %>% add_row(hello = "hola", goodbye = "ciao")

