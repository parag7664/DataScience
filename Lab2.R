x <- seq(-4,2,0.05)
y <- 2^x
plot(x,y,"l")
plot(x,y)

   
x <- seq(-4,4,0.05)
y <- (1/2)^x
plot(x,y,"l",col='blue')


x <- seq(-4,4,0.05)
g <- 2^x
h <- (1/2)^x
plot(x,g,'l',col='red')
lines(x,h,"l",col='blue')

x <- seq(-4,4,0.05)
a <- 2^-x
b <- 2^x
c <- -2^x
d <- (1/2)^x
e <- (-2)^x

x <- seq(-4,4,0.05)
a <- 3^-x
b <- 3*x
c <- x^3
d <- 3^2*x
e <- x+3
f <- 3^(x)^2
plot(x,f,'l')
library("ggplot2")

ggplot()+geom_point(mapping = aes(x,a,col='red'))+geom_point(mapping = aes(x,b))+geom_point(mapping = aes(x,c))

p <- seq(0,21)
r <- -5*p^2+105*p
plot(p,r,'l',col='red')
                                                             
                                                             
