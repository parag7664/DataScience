install.packages("foreign")
library(foreign)

data <- read.spss("C:\\Users\\Francisco.Hernandez\\OneDrive - Cork Institute of Technology\\CIT LECTURES\\STAT9005 TS n FA\\Project\\PCA_STAT9005 Project.sav", to.data.frame=TRUE)

data <- read.spss("C:\\Users\\Francisco.Hernandez\\OneDrive - Munster Technological University\\CIT LECTURES\\STAT9005 TS n FA\\2022\\Project\\FA_STAT9005_Project.sav", to.data.frame=TRUE)

attach(data)
#Manufacturer
head(data, n=4L)
data[57,]
#?read.spss
#data$Model 
#?head
dcars_origin = data[-57,-c(13,16)] ## Leave Origin and Type
dcars = data[-57,-c(1,3,13,16)]  ## remove Categorical / Text except Brand& model
                         ## (57)remove mazda RX7 with "Rotative" cylinders
                         ## Other option --> asign a value to Rotative = 6 e.g.
head(dcars_origin, n=1L)
head(dcars, n=1L)
dcars[57,]

## create var for row.names with Brand&Model
#?paste
n = dim(dcars)[1]
model = NULL; brand=NULL; car=NULL
for(i in 1:n ){ ## create var for row.names with Brand&Model
    brand = rbind(brand, dcars[i,11] )
    model = rbind(model, dcars[i,12] )
    car = rbind(car, paste( dcars[i,11], dcars[i,12], sep=" ") )
              }
#cars = cbind(dcars[,11:12], dcars[,1:10] )
#?data.frame

## Create new dataframe with row names "cars"
cars_origin.df = data.frame(cbind (dcars_origin[,c(1,3)], dcars[, 1:10]), 
                                    row.names=car)  
head(cars_origin.df,n=1L)

cars.df = data.frame(dcars[,1:10], row.names=car)  
head(cars.df,n=1L)

## Correlation
plot(cars.df)
# Why is there a problem with pairs? - Explain
# Transform the data to apply "pairs"
pairs(cars.df)## have to convert Variable "Cylinders" to numeric
head(cars.df)
Cylinders
# Error in pairs.default(dcars) : non-numeric argument to 'pairs'
cars.df[,7]  ## Still is categorical -> need to transform as numeric
cars.df[,7] = sapply(dcars[,7],as.numeric)
cars_origin.df[,9] = cars.df[,7]

pairs (cars.df)  ## Now is working as all vars are numerical

## Remove Price and Passengers from dataset
#cars.df = cars.df[,-c(1,9)]; head(cars.df)
#cars_origin.df = cars_origin.df[,-c(3,11)] ;head(cars_origin.df)
#########################################

nv = dim(cars.df)[2]
attach(cars.df)  ## attach dataframe
