Edu_salary <- read.csv("D:/Sem 1/Maths method and modelling Math8009_24011/Edu_salary.csv")
View(Edu_salary)
attach(Edu_salary)
plot(Salary, Years, main = "salary vs years", xlab = "salary", ylab = "years", col="red")
plot(Salary, Rating, main = "salary vs Rating", xlab = "salary", ylab = "Rating", col="red")


yearsModel <- lm(Salary~Years)     
yearsModel
abline(yearsModel, col="blue")
summary(yearsModel)


ratingModel <- lm(Salary~Rating)
ratingModel
abline(ratingModel, col="blue")
summary(ratingModel)


#lesss scattered values means more cor coeff. and strong +ve linear relationship
sal_correlation <- cor(Salary, Years)
sal_correlation # this is high, meaning close to 1 so strong +ve linear relationship
sal_det_coeficient <- sal_correlation^2
sal_det_coeficient # 75.27% of variation in salary can be explained by years
summary(Salary)

rat_correlation <- cor(Salary, Rating)
rat_correlation # this is moderate, meaning not so close to 1 so moderate +ve linear relationship
rat_det_coeficient <- rat_correlation^2
rat_det_coeficient # 29 % of variation in salary can be explained by rating
summary(Rating)
 

# hypothesis test:
adjusted r sqpare - allows to sample size
coeff estimate = regression of coefficients

first model applicable to pop 
H0 = there is no linear relationship between years and Salary

alternate hypothesis
Ha = there is a linear relationship between Years and Salary
p values prob of observing a relationship and it being down to chance variation
p = 7.27e-07 <0.05 reject ho, accept ha, there is a linear relationship.


Intercep hypothesis test 
H0 : a =0
Ha : a !=0


f stats     (hypothesis test )
H0 - all the regression coefficients are equal to 0 a= b= 0
Ha - there is atleast one regression coefficients that i snon zero -  used in multiple linear regression
p value 7.27e-07 <0.05 
        reject H0
        accept Ha
        
,