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

pairs(decathlon2.active)

#checking whether data reduction is required or not
#test provides probability that the correlation matrix has significant correlations among at least some of the variables in a dataset
check_sphericity_bartlett(decathlon2.active)

#Compute PCA using prcomp
res.pca <- prcomp(decathlon2.active, scale = TRUE)
summary(res.pca)
#Compute PCA using PCA
pca1 = PCA(decathlon2.active, graph = TRUE, scale.unit = TRUE)
pca1

# matrix with eigenvalues
pca1$eig
pca1$var  ## coordinates of vars in PCs
# correlations between variables and PCs
pca1$var$coord   # (eigen value * sqrt(eigen vector))
# PCs (aka scores)
head(pca1$ind$coord)


#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component
fviz_eig(res.pca)

#ggbiplot(res.pca)
biplot(res.pca, scale = 0)

#Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 



#scree plot - a plot that displays the total variance explained by each principal component - to visualize the results of PCA
##  EFA 
parallel <- factanal(decathlon2.active, factors = 3)


# Parallel analysis suggests that the number of 
# factors =2 and the number of components = NA


# Now that we know how many factors we need, we can perform 
# the factor analysis using the fa() function.
factors <- fa(decathlon2.active,nfactors=2,rotate='varimax',fm='minres')
print(factors)

##  EFA 
parallel <- fa.parallel(decathlon2.active,fm="minres",fa='fa')
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



