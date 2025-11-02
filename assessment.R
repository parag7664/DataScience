library(tidyverse)
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")

install.packages("GPArotation")
library(GPArotation)
install.packages()
library(psych)
library(corrplot)


install.packages("parameters")
library(parameters) 
install.packages("devtools")
library(devtools)

install.packages("ggbiplot")
library(ggbiplot)

# PCA with function dudi.pca
install.packages("ade4")
library(ade4)

# PCA with function PCA
install.packages("FactoMineR")
library(FactoMineR)

install.packages("factoextra")
library("factoextra")
#################################################

# loading data
data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])

#find correlation
cor_matrix <- cor(decathlon2.active)
cor_matrix

# Exploring data
mtcars
head(mtcars)
#test provides probability that the correlation matrix has significant correlations among at least some of the variables in a dataset
check_sphericity_bartlett(mtcars) 


###### PCA
# calculating principal component
# with this mtcars[,c(1:7,10,11)] removing categorical variables
mtcars.pca<-prcomp(mtcars[,c(1:7,10,11)],center=TRUE, scale.=TRUE)
mtcars.pca
summary(mtcars.pca)

#using PCA
pca1 = PCA(mtcars[,c(1:7,10,11)], graph = TRUE, scale.unit = TRUE)
pca1

# matrix with eigenvalues
pca1$eig
pca1$var  ## coordinates of vars in PCs
# correlations between variables and PCs
pca1$var$coord   # (eigen value * sqrt(eigen vector))
# PCs (aka scores)
head(pca1$ind$coord)

# apply PCA
pca4 = dudi.pca(mtcars[,c(1:7,10,11)], nf = 2, scannf = FALSE)
pca4 =dudi.pca(mtcars[,c(1:7,10,11)], nf = 2, scannf = TRUE)
pca4
#
pca4$eig
pca4$tab
pca4$co
print(pca4)
score(pca4)
s.corcircle(pca4$co)

scatter(pca4)

s.label(pca4$li)

#
# eigenvalues
pca4$eig
## loadings
pca4$c1
# correlations between variables and PCs
pca4$co
# PCs
head(pca4$li)


#if eigenvectors in R point in the negative direction by default, so we'll multiply by -1 to reverse the signs.
#reverse the signs
#results$rotation <- -1*results$rotation

#Visualize the Results with a Biplot
ggbiplot(mtcars.pca)
plot(mtcars.pca)
biplot(mtcars.pca, scale = 0)

#calculate total variance explained by each principal component
mtcars.pca$sdev^2 / sum(mtcars.pca$sdev^2)




#scree plot - a plot that displays the total variance explained by each principal component - to visualize the results of PCA
##  EFA 
parallel <- fa.parallel(mtcars[,c(1:7,10,11)],fm="minres",fa='fa')

# Parallel analysis suggests that the number of 
# factors =2 and the number of components = NA


# Now that we know how many factors we need, we can perform 
# the factor analysis using the fa() function.
fa(mtcars)
factors <- fa(mtcars[,c(1:7,10,11)],nfactors=2,rotate='varimax',fm='minres')
print(factors)

#apply(factors$loadings^2,1,sum) # communality
#1- apply(factors$loadings^2,1,sum) # uniqueness
#scores
factors$scores

#plotting
plot(factors)
biplot(factors)

#loading
factors$loadings

# Create a path diagram of the items' factor loadings
fa.diagram(factors)
