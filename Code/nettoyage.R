library(data.table)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(rgl)
library(caret)

# Chargement des donn?es avec fread, plus intelligent que read.csv
climat <- fread("Données/climat.201708.csv",data.table = F)
villes <- fread("Données/postesSynop.csv",data.table=F)
climat <- merge(climat,villes,by="NUM_POSTE")
row.names(climat) <- climat$Nom
climat <- climat[,-c(1,55)]
# R?cup?rer variables avec plus de 10% de NA pour les enlever
idx_bad_col <- which(apply(climat,2,function(x){sum(is.na(x))})> (dim(climat)[1]/10))
# elever les variables avec variance trop faible, probl?me pour l'acp sinon
near_zero_var <- nzv(climat, freqCut = 90/10)
# et les enlever ainsi que FXAB qui doit ?tre mise de c?t?
climat_rm_na <- climat[,-c(idx_bad_col,near_zero_var,which(colnames(climat)=="FXAB"))]
# Virer lignes avec au moins 1 NA
idx_bad_row <-  which(apply(climat_rm_na,1,function(x){sum(is.na(x))}) > 0)
# On retrouve 9 lignes avec au moins 1 NA, on r?cup?re un tableau de 46 lignes sans NA
climat_rm_na <- climat_rm_na[-idx_bad_row,]
