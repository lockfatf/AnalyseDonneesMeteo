library(data.table)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(rgl)
library(caret)

# On suppose que toutes les donn?es et librairies ont ?t? charg?es dans l'?tape pr?c?dente (nettoyage)
featurePlot(x=preproc_impute$completeObs)
# ACP test pre-exploratoire
res_pca_vire <- PCA(preproc_impute$completeObs, ncp = dim(preproc_impute$completeObs)[2], scale.unit = TRUE, graph=F)
#plot3d(res_pca$ind$coord[,1:3])
#fviz_pca_ind(res_pca,axes=c(1,2),repel = T)
fviz_contrib(res_pca_vire,axes=c(1:6),choice="ind")
fviz_contrib(res_pca_vire,axes=c(1:6),choice="var")
contrib_ind <-fviz_contrib(res_pca_vire,axes=c(1:6),choice="ind")
#ggplot(data=climat_rm_na) +  geom_point(mapping = aes(x = NBJFF10, y = NBJFF20))

ind_high_contrib <- rownames(contrib_ind$data[contrib_ind$data$contrib>1.5,]) 

res_pca2 <- PCA(preproc_impute$completeObs[-which(rownames(preproc_impute$completeObs) %in% ind_high_contrib),], ncp = dim(preproc_impute$completeObs)[2], scale.unit = TRUE, graph=F)
fviz_pca_biplot(res_pca2,axes=c(1,2),repel = T,col.ind = "cos2")
plot3d(res_pca2$ind$coord[,1:3])
fviz_contrib(res_pca2,axes=c(1:6),choice="ind")
fviz_contrib(res_pca2,axes=c(1:6),choice="var")

preproc <- as.data.frame(preproc_impute$completeObs[-which(rownames(preproc_impute$completeObs) %in% ind_high_contrib),])
preproc$NUM_POSTE <- climat_rm_na$NUM_POSTE[-which(rownames(climat_rm_na) %in% ind_high_contrib)]
