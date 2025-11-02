retire_Age <- c(57,62,60,57,65,60,58,62,56)
death_age <- c(71,70,66,70,69,67,69,63,70)

plot(retire_Age, death_age, main="scatter plot", xlab = "retire age", ylab = "death age", col="red")
modelQ1 <- lm(retire_Age~death_age)
modelQ1

abline(lm(death_age~retire_Age), col="blue")

correlation <- cor(retire_Age, death_age)
det_coeficient <- correlation^2
det_coeficient

summary(retire_Age)
summary(death_age)

hist(retire_Age, xlab="retire age", ylab = "frequency",main = paste("Histogram of retire_Age"))
hist(death_age, xlab="death age", ylab = "frequency",main = paste("Histogram of death_age"))


boxplot(retire_Age, death_age, border = par("fg"), col = "lightgray", log = "",
        pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),  horizontal = FALSE, add = FALSE)

#question 2
auto_sales <- read.csv("D:/Sem 1/Maths method and modelling Math8009_24011/auto_sales.csv")
View(auto_sales)

library(DataExplorer)
summary(auto_sales)
attach(auto_sales)
plot(sales, outlets, main="scatter plot", xlab = "sales", ylab = "outlets", col="red")
lm(outlets~sales)
abline(lm(outlets~sales), col="blue")
plot(sales, reg_auto, main="scatter plot", xlab = "sales", ylab = "reg_auto", col="red")
lm(reg_auto~sales)
abline(lm(reg_auto~sales), col="blue")
plot(sales, Per_ncome, main="scatter plot", xlab = "sales", ylab = "Per_ncome", col="red")
abline(lm(Per_ncome~sales), col="blue")
plot(sales, Av_age_auto, main="scatter plot", xlab = "sales", ylab = "Av_age_auto", col="red")
abline(lm(Av_age_auto~sales), col="blue")
plot(sales, supervisors, main="scatter plot", xlab = "sales", ylab = "supervisors", col="red")
abline(lm(supervisors~sales), col="blue")

