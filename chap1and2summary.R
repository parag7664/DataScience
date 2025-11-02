h =  seq (1,24,by = 1)
Z <-   array( h,dim = c(3,4,2))
Z

Z[1,1,1]
Z[1,2,2]

lst <- list(name="Fred", wife="Mary", no.children =3,child.ages=c(4,7,9))
lst
lst[[2]] 
lst[[4]][1]
length(lst)

x = c(88, NA, 12, 168, 13)
mean(x)
mean(x, na.rm=TRUE)

Y = c(88, NULL, 12, 168, 13)
mean(Y)

z=NULL
for(i in 1:5){z=c(z,i)}; 
z

z=NA
for(i in 1:5){z=c(z,i)}; 
z

read.table("D:/Sem 1/Intro to R and data science stats8010_27331/Lab2/brainsize.txt")

X <-   matrix(scan("patient.dat"), ncol =4, byrow =TRUE)

1 == 1
1 == 2
1 != 2
1 <= 2 & 1 <= 3
1 == 1 | 1 == 2
1 > 1 | 1 > 2 & 3 == 3
1 > 1 & 1 > 2 & 1 > 3

i <- 1
if(i==1){print('one')}
i=2
if(i==1)  { print("one") }
i=11
while(i <= 10){
  print(
    i )
}

v <- c('Hello','repeat loop')
cnt <- 0
repeat { print(v)
  cnt
  cnt+1
  if(cnt >5) {
    break; #see below
  }
}

squared <-   function(x){ x*x }
A=squared(4)

x <- c(1,2,3,4,5,6,7,8)
y <- c(8,7,6,5,4,3,2,1)
quantile(x)
quantile(x,1)
quantile(x,0.37)

mean(x)
# mean of x
median(x)
# median of x
sd( x)# standard deviation of x
var(x)# variance of x
IQR(x)# Interquartile range of x
cov(x)# covariance of x
range(x)# range of x
sum(x)# sum of x
diff(x)# First Differences in x

plot(x)# plot of x (on the y axis ) ordered on the x axis
plot(x, y)# bivariate plot of x (on x axis ) and y (on y axis )
hist(x) # histogram of the frequencies of x
barplot(x)# histogram of the values of x; use horiz TRUE for horizontal bars
matplot( x,y ) # plot of columns of matrices
boxplot(x)# "box and whiskers" plot
stem(x)# stem&leaf plo t plot
dotchart(x)# if x is a data frame, stacked plots line by line and column by column.
pie(x)
sunflowerplot(x, y) #
stripchart(x)

points(x, y)# adds points to a plot (the option type= can be used)
lines(x, y)# as with points but with lines
(use of  matpoints & matlines for matplot of matrices)
text(x, y, labels) # adds text given by labels at coordinates x,y 
abline(a,b )# draws a line of slope b and intercept a.
abline(h=y) draws a horizontal line at ordinate y
abline(v= x)# draws a vertical line at abcisa x.
abline(lm.obj) draws the regression line given by the model "lm.obj"
  
rect  (3, 3, 5, 5)

subset(iris, Species == 'virginica')  

order(x)
iris [order(iris$Petal.Length ),] #assc
iris [order(-iris$Petal.Length ),] #Desc

hc <- hclust (dist(USArrests ), 'ave')
plot(hc)

USArrests[1,]=50
hc <- hclust (dist (USArrests ), 'ave')
plot(hc)


xx = iris[,1:4]
xx
prcomp(xx) # inappropriate
prcomp(xx, scale. = TRUE)
par(mfrow=c(1,2)
plot(prcomp(xx))
plot(prcomp (xx, scale. = TRUE))
summary(prcomp (xx))
summary(prcomp (xx, scale. = TRUE))
biplot(prcomp (xx), main="RAW DATA")
biplot(prcomp (xx, scale. = TRUE), main="SCALED DATA")

