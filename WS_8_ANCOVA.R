library(tidyverse)

# read in data and store in a tibble
fruit<-read_tsv("fruit.txt")


# create a scatterplot plot using different shaped points
# for grazed and ungrazed
windows(8,6)
ggplot(fruit, aes(x = Root, y= Fruit)) +
  geom_point(aes(shape = factor(Grazing)), size = 2) 

# create a boxplot for each level of grazing. Note that it seems 
# plants from the grazed treatment produced more fruit.
fruit$Grazing<-as.factor(fruit$Grazing)
plot(Fruit ~ Grazing, data = fruit)

# a t-test shows that fruit production is higher for the
# grazed treatment - this conclusion is wrong since it does not account for
# the relationship between inizial plant size and fruit production
t.test(Fruit ~ Grazing, data = fruit)

# fit the ANCOVA model including an interaction term
# (see  notes for fitted model and interpretations)
m1<-lm(Fruit~Root*Grazing, data=fruit)
summary(m1)


# Test the hypothesis:
# H0: β1 = β2 = β3
# HA: at least one of the βi ≠ 0
# What do the results of the hypothesis test imply for the regression model?
# The summary output from the model contains information regarding the global
# F-test comparing the fitted model to the null model (intercept only)
# The F-statistic and associated p-value indicate that at lrast one of the predictors is 
# associated with the weight of fruit produced by the plants


# Test the hypothesis:
# H0: β3 = 0
# HA: β3 ≠ 0
# What do the results of the hypothesis test imply for the regression model?


m2<-lm(Fruit~Root+Grazing, data=fruit)
summary(m2)
anova(m2,m1)

# since the p-value associated with the interaction term is < 0.05, then at
# the 5% confidence  level the data indicates that there is not an interaction 
# between root and grazing, i.e. the relationship between fruit and root does
# not depend on the level of grazing. The regression lines are parallel.

# To plot the regression lines onto the model:
windows(8,6)
ggplot(fruit, aes(Root, Fruit)) +
  geom_point(aes(shape = factor(Grazing)), size = 2) +
  geom_abline(intercept=-127.829 ,slope=23.56) +
  geom_abline(intercept=-91.726 ,slope=23.56) 

# Diagnotics
windows(10,10)
par(mfrow=c(2,2))
plot(m2)

#####################################################################
Birds<-read.table("Birds.txt",header=T)


######## Birds Analysis
####### GRAZE IS A FACTOR##########
levels(Birds$GRAZE)
Birds$GRAZE<-as.factor(Birds$GRAZE)

########## Exploratory Data Analysis############

summary(Birds$ABUND)

# Note that Area, Distance and L Distance are skewed
boxplot(Birds$ABUND, main="Abundance")
boxplot(Birds$AREA, main="Area")
boxplot(Birds$DIST, main="Distance")
boxplot(Birds$LDIST, main="L Distance")
boxplot(Birds$YR.ISOL, main="Years")
boxplot(Birds$ALT, main="Altitude")


hist(Birds$ABUND, main="Abundance")
hist(Birds$AREA, main="Area")
hist(Birds$DIST, main="Distance")
hist(Birds$LDIST, main="L Distance")
hist(Birds$YR.ISOL, main="Years")
hist(Birds$ALT, main="Altitude")

########### Examine Relationships Beween Variables #########
## To produce pairs plot

Z <- cbind(Birds$ABUND, Birds$AREA, Birds$DIST, Birds$LDIST, Birds$YR.ISOL, Birds$ALT, Birds$GRAZE)

colnames(Z) <- c("ABUND", "AREA", "DIST",
                 "LDIST", "YR.ISOL", "ALT", "GRAZE")

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# No obvious relationships between variables. 
windows(10,10)
pairs(Z, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist)

# Try transforming Area, Distance and L Distance
hist(log10(Birds$AREA))
hist(log10(Birds$DIST))
hist(log(Birds$LDIST))

Birds$LOGAREA<-log10(Birds$AREA)
Birds$LOGDIST<-log10(Birds$DIST)
Birds$LOGLDIST<-log(Birds$LDIST)


# re-examine scatterplots
Z2 <- cbind(Birds$ABUND, Birds$LOGAREA, Birds$LOGDIST, Birds$LOGLDIST, Birds$YR.ISOL, Birds$ALT, Birds$GRAZE)

colnames(Z2) <- c("ABUND", "LOGAREA", "LOGDIST",
                  "LOGLDIST", "YR.ISOL", "ALT", "GRAZE")

windows(10,10)
pairs(Z2, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist)


#### Refit model based on transformed variables
model1<- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST + YR.ISOL +ALT + GRAZE, data = Birds)

summary(model1)

plot(model1)


######  Plot regression lines for the model Abund~ Log(AREA) + Graze #######

model2<-lm(ABUND ~ LOGAREA + GRAZE, data = Birds)
windows(10,10)
plot(Birds$LOGAREA,Birds$ABUND)

## extract data for each level of grazing

D1 <- data.frame(LOGAREA = Birds$LOGAREA[Birds$GRAZE==1],   GRAZE = "1")
D2 <- data.frame(LOGAREA = Birds$LOGAREA[Birds$GRAZE==2],   GRAZE = "2")
D3 <- data.frame(LOGAREA = Birds$LOGAREA[Birds$GRAZE==3],   GRAZE = "3")
D4 <- data.frame(LOGAREA = Birds$LOGAREA[Birds$GRAZE==4],   GRAZE = "4")
D5 <- data.frame(LOGAREA = Birds$LOGAREA[Birds$GRAZE==5],   GRAZE = "5")

## predict values using the fitted model and the explanatory variables found in each level of grazing
## so for P1 grazing is set to 1 for all datapoints etc

P1 <- predict(model2, newdata = D1)
P2 <- predict(model2, newdata = D2)
P3 <- predict(model2, newdata = D3)
P4 <- predict(model2, newdata = D4)
P5 <- predict(model2, newdata = D5)

lines(D1$LOGAREA, P1, lty = 1)
lines(D2$LOGAREA, P2, lty = 2)
lines(D3$LOGAREA, P3, lty = 3)
lines(D4$LOGAREA, P4, lty = 4)
lines(D5$LOGAREA, P5, lty = 5)

