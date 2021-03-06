library(data.table)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(rgl)
library(caret)

# On suppose que toutes les donn?es et librairies ont ?t? charg?es dans l'?tape pr?c?dente (nettoyage)

# ACP test pre-exploratoire
res_pca_vire <- PCA(preproc_impute$completeObs, ncp = dim(preproc_impute$completeObs)[2], scale.unit = TRUE, graph=F)
#plot3d(res_pca$ind$coord[,1:3])
#fviz_pca_ind(res_pca,axes=c(1,2),repel = T)
fviz_pca_biplot(res_pca_vire,axes=c(1,2),repel = T)
#ggplot(data=climat_rm_na) +  geom_point(mapping = aes(x = NBJFF10, y = NBJFF20))
idx_high_contrib1 <- which(res_pca_vire$ind$contrib[,1]>10)

res_pca2 <- PCA(preproc_impute$completeObs[-idx_high_contrib1,], ncp = dim(preproc_impute$completeObs)[2], scale.unit = TRUE, graph=F)
fviz_pca_biplot(res_pca2,axes=c(1,2),repel = T)

preproc <- as.data.frame(preproc_impute$completeObs[-idx_high_contrib1,])
