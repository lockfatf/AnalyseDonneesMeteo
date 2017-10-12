library(data.table)
library(tidyverse)
library(FactoMineR)
library(factoextra)

# Chargement des données avec fread, plus intelligent que read.csv
climat <- fread("climat.201708.csv",data.table = F)
# Récupérer variables avec plus de 10% de NA
idx_bad_col <- which(apply(climat,2,function(x){sum(is.na(x))})> (dim(climat)[1]/10))
# et les enlever
climat_rm_na <- climat[,-c(idx_bad_col,which(colnames(climat)=="FXAB"))]

idx_bad_row <-  which(apply(climat_rm_na,1,function(x){sum(is.na(x))}) > 1)
# On retrouve 9 lignes avec des NA, on peut récupérer un tableau de 49 lignes au lieu de 58 sans aucun NA!
climat_rm_na <- climat_rm_na[-idx_bad_row,]

# ACP test pré-exploratoire
res_pca <- PCA(climat_rm_na %>% select_if(is.numeric), ncp = 10, scale.unit = TRUE, graph=T)
