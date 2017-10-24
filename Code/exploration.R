library(data.table)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(rgl)
library(caret)

# On suppose que toutes les donn?es et librairies ont ?t? charg?es dans l'?tape pr?c?dente (nettoyage)

#on vire les donné ressorti pas findCorrelation
climat_rm_na_vire = climat_rm_na[,-vire_var]
# ACP test pr?-exploratoire
res_pca <- PCA(climat_rm_na %>% select_if(is.numeric), ncp = 30, scale.unit = TRUE, graph=F)

# ACP test pr?-exploratoire
res_pca_vire <- PCA(climat_rm_na_vire %>% select_if(is.numeric), ncp = 30, scale.unit = TRUE, graph=F)
#plot3d(res_pca$ind$coord[,1:3])
#fviz_pca_ind(res_pca,axes=c(1,2),repel = T)
fviz_pca_biplot(res_pca,axes=c(1,2),repel = T)
#ggplot(data=climat_rm_na) +  geom_point(mapping = aes(x = NBJFF10, y = NBJFF20))
