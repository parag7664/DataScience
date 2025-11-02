###########Import libraries ###########################

#install.packages("foreign")
library(foreign)
library(ade4)
library(factoextra)
library(magrittr)
library(FactoMineR)
library(tidyverse)
library(psych)
library(factoextra)
#install.packages("nFactors")
library(nFactors)
library("scatterplot3d") # load
require(graphics)


#########################################################################################################

setwd("D:/Sem 2/Time Series and factor analysis/assignment 2")
car_data <- read.spss("FA_STAT9005_Project.sav", to.data.frame=TRUE)
View(car_data)
str(car_data)
attach(car_data)


head(car_data, n=4L)
car_data[57,]

## remove 57 row mazda RX7 with "Rotary" cylinders
cars_data_subset1 = car_data[-57,-c(13,16)] ## Leave Airbags and inclass
View(cars_data_subset1)

## remove Categorical (type and origin) / Text except Brand& model
cars_data_without_categorical = car_data[-57,-c(1,3,13,16)]  
View(cars_data_without_categorical)


head(cars_data_subset1, n=1L)
head(cars_data_without_categorical, n=1L)
cars_data_without_categorical[57,]

## create var for row.names with Brand&Model
n = dim(cars_data_without_categorical)[1] #n = 92

model = NULL; brand=NULL; car=NULL

for(i in 1:n ){ ## create var for row.names with Brand&Model
  brand = rbind(brand, cars_data_without_categorical[i,11] )
  model = rbind(model, cars_data_without_categorical[i,12] )
  car = rbind(car, paste( cars_data_without_categorical[i,11], cars_data_without_categorical[i,12], sep=" ") )
}


## Creating new car_dataframe with row names "cars"
new_df = data.frame(cbind (cars_data_subset1[,c(1,3)], cars_data_without_categorical[, 1:10]), 
                            row.names=car)  
head(new_df,n=1L)

#Removed origin column from dataset.
cars.df = data.frame(cars_data_without_categorical[,1:10], row.names=car)  
head(cars.df,n=1L)

## Correlation
pairs(cars.df)## have to convert Variable "Cylinders" to numeric because of # Error in pairs.default(cars_data_without_categorical) : non-numeric argument to 'pairs'

library(corrplot)
m <- cor(cars.df)
corrplot(m, method = "number")

head(cars.df)
str(cars.df)

#converting Variable "Cylinders" to numeric
cars.df[,7]
cars.df[,7] = sapply(cars_data_without_categorical[,7],as.numeric)
new_df[,9] = cars.df[,7]

pairs(cars.df)
attach(cars.df)
View(cars.df)
#########################################################################3#####

## Plotting 3 pairs of variable
plot(Horsepower ~ Price, main= "Horsepower VS Price")
l1<-lm(Horsepower ~ Price)
abline(l1)

plot(EngineSize ~ RPM, main = "Engine size VS RPM")
l2<-lm(EngineSize ~ RPM)
abline(l2)

plot(Fuel.tank.capacity ~ Weight, main = "Fuel tank capacity VS weight")
l3<-lm(Fuel.tank.capacity ~ Weight)
abline(l3)

###############################################################################
## 2D regression plane on 3D plot
s3d <- scatterplot3d(MPG.city,Cylinders,EngineSize, type = "p",highlight.3d = TRUE, pch = 20, main="mpgcity ~ cylinders + enginesize")
# Add regression plane
mod <- lm(MPG.city ~ Cylinders + EngineSize, data=cars.df)
s3d$plane3d(mod,draw_polygon = TRUE, draw_lines = TRUE)

s3d1 <- scatterplot3d(Horsepower,Cylinders,EngineSize, type = "p",highlight.3d = TRUE, pch = 20, main="horsepower ~ cylinders + enginesize")
# Add regression plane
mod2 <- lm(Horsepower ~ Cylinders + EngineSize, data=cars.df)
s3d1$plane3d(mod2,draw_polygon = TRUE, draw_lines = TRUE)

###############################################################################

#Bartlett test to test car_dataset and check whether is suitable for data reduction techniques.
cor_matrix <- cor(cars.df)
cortest.bartlett(cor_matrix, n = nrow(cars.df))

#The Chi-Square test statistic is 1101.148
#p-value is 2.860564e-201, 
#p-value < significance level (let's say 0.05). 
#Thus, this car_data is suitable for for data reduction techniques (PCA or factor analysis). 

###############################################################################
# PCA
#calculate principal components
str(cars.df)
cars_pca <- prcomp(cars.df, center=TRUE, scale = TRUE)
cars_pca
summary(cars_pca)
str(cars_pca)


# eigen values using prcomp
eigen_values = (cars_pca$sdev)^2; 
eigen_values

#scores
head(cars_pca$x, 10) 

#sum
eigen_values/sum(eigen_values)

#cumulative sum
cumsum(eigen_values)/sum(eigen_values)

###############################################################################

#screeplot
fviz_eig(cars_pca)
screeplot(cars_pca, type="line", main="Screeplot")

library("ggbiplot")
ggbiplot(cars_pca, scale = 0)
ggbiplot(cars_pca, labels=rownames(cars.df))

###############################################################################

### loadings
#  Loadings=Eigenvectors * squareroot(eigenvalues)
cars_pca$rotation
l = head(cars_pca$rotation) ## show the loadings: weight of the var in the component
l

###############################################################################


# factomineR factoextra library usa bioplot and non usa bioplot
#Using factominR and factoextra library we have plot bioplot for usa and non-usa manufacture car. 
windows(10,10)
par(mfrow=c(3,3))

df = split(new_df, f = factor(new_df$Origin))


#df$USA
#head(data_usa)
usa_cars_data = df$USA[,-c(1,2)]
#non_data = df$non-USA[,-c(1,2)]

us_cars_pca <- prcomp(usa_cars_data, center=TRUE, scale = TRUE)
us_cars_pca
summary(us_cars_pca)

non_usa_cars_data = df$`non-USA`[,-c(1,2)]
non_us_cars_pca <- prcomp(non_usa_cars_data, center=TRUE, scale = TRUE)
non_us_cars_pca
summary(non_us_cars_pca)

fviz_eig(us_cars_pca, title="scree plot for usa origin cars")

fviz_eig(non_us_cars_pca, title="scree plot for Non - usa origin car")

fviz_pca_ind(us_cars_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), title = "pca individual usa origin cars",
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(non_us_cars_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), title = "pca individual for Non-US origin cars",
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(us_cars_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), title = "pca variability for usa origin",
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(non_us_cars_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), title ="pca variability for Non-US origin cars",
             repel = TRUE     # Avoid text overlapping
)

########################################################################################################################

# Factor Analysis
fit <- factanal(cars.df, 2, rotation="varimax")
fit
fit<-factor.pa(cars.df, nfactors=2, rotate="varimax")
fit

#single-factor parallel
EFA_model <- fa(cars.df)
parallel_model <- fa.parallel(cars.df,fm="minres",fa='fa')
efa_model2 <- fa(cars.df, nfactors=2)
fa.diagram(efa_model2)

efa_model3 <- fa(cars.df, nfactors=3)
fa.diagram(efa_model3)


########################################################################################################################
# PCA and FA analysis for only usa

summary(us_cars_pca)

# eigen values using prcomp
eigen_values_usa = (us_cars_pca$sdev)^2; 
eigen_values_usa
head(us_cars_pca$x, 10) #scores

#screeplot
plot(us_cars_pca)
screeplot(us_cars_pca, type="line", main="Screeplot")
biplot(us_cars_pca, scale = 0)


### loadings
l_usa = head(us_cars_pca$rotation) ## show the loadings: weight of the var in the component
l_usa


### Factor analysis ###
fit_usa <- factanal(usa_cars_data, 2, rotation="varimax")
fit_usa
fit_usa<-factor.pa(usa_cars_data, nfactors=2, rotate="varimax")
fit_usa

# Conduct a single-factor parallel
# EFA_model_usa <- fa(usa_cars_data)
# fa.diagram(EFA_model_usa)
parallel_model_usa <- fa.parallel(usa_cars_data,fm="minres",fa='fa')
efa_model3_usa <- fa(usa_cars_data, nfactors=2)
fa.diagram(efa_model3_usa)

########################################################################################################################

# Factor analysis model Full dataset and only USA manufacturing dataset comparision
fa.diagram(efa_model2, main  = "factor analysis full dataset")
fa.diagram(efa_model3_usa, main = "factor analysis usa dataset")

# code Referenced from:
# Class notes
# http://strata.uga.edu/8370/lecturenotes/principalComponents.html
# https://blog.bioturing.com/2018/06/18/how-to-read-pca-biplots-and-scree-plots/
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
# https://blogs.sas.com/content/iml/2019/11/04/interpret-graphs-principal-components.html
# https://support.minitab.com/en-us/minitab/19/help-and-how-to/graphs/3d-scatterplot/interpret-the-results/key-results/