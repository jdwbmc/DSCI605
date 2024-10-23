# install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR)
library(factoextra)
library(tidyverse)

data(decathlon2)  #in "FactoMineR" package
View(decathlon2)

###subset data
decathlon2.active <- decathlon2[1:23, 1:10]
view(decathlon2.active)

##Conduct PCA
res.pca <- PCA(decathlon2.active,scale.unit = TRUE,ncp = 5,graph = FALSE)
res.pca
eig.val <- get_eigenvalue(res.pca)
eig.val

##Visualize new variables :principle components
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))
##Visualize the relationship of original variables and new variables component principles
fviz_pca_var(res.pca, col.var = "black")

##Visualize single observation in new coordinate space of component principles
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Create a random continuous variable of length 23,
# Same length as the number of active individuals in the PCA

set.seed(23)
my.cont.var <- rnorm(23)
# Color individuals by the continuous variable
fviz_pca_ind(res.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

