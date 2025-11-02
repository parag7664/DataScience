loyalty <- read.csv("D:/Sem 1/App stats and prob/lab 4/loyalty.csv")
View(loyalty)

loyalty$gender2[loyalty$ï..Gender==1] <- "Male"
loyalty$gender2[loyalty$ï..Gender==2] <- "Female"

loyalty$Payment2[loyalty$Payment==0] <- "Credit Card"
loyalty$Payment2[loyalty$Payment==1] <- "Cash"

loyalty$LoyaltyCard2[loyalty$LoyaltyCard==1] <- "Yes"
loyalty$LoyaltyCard2[loyalty$LoyaltyCard==2] <- "No"
View(loyalty)

#basic descriptive
table(loyalty$gender2)
table(loyalty$Payment2)
table(loyalty$LoyaltyCard2)

boxplot(loyalty$Payment~loyalty$gender2)
attach(loyalty)
table(gender2,Payment2)
