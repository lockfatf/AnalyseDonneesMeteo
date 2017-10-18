# On suppose que toutes les données et librairies ont été chargées dans l'étape précédente (nettoyage)

# ACP test pré-exploratoire
res_pca <- PCA(climat_rm_na %>% select_if(is.numeric), ncp = 30, scale.unit = TRUE, graph=F)
#plot3d(res_pca$ind$coord[,1:3])
#fviz_pca_ind(res_pca,axes=c(1,2),repel = T)
fviz_pca_biplot(res_pca,axes=c(1,2),repel = T)
#ggplot(data=climat_rm_na) +  geom_point(mapping = aes(x = NBJFF10, y = NBJFF20))
