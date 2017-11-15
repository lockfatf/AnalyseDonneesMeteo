library(data.table)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(rgl)
library(caret)
library(missMDA)

#pour flo : setwd("/users/florianlockfat/Documents/GitHub/AnalyseDonneesMeteo")

# Chargement des donnees avec fread, plus intelligent que read.csv
climat <- fread("Données/climat.201708.csv",data.table = F)
villes <- fread("Données/postesSynop.csv",data.table=F)
climat <- merge(climat,villes,by="NUM_POSTE",all.x = T)
climat$Nom[is.na(climat$Nom)] <- "Nom Inconnu"
row.names(climat) <- climat$Nom
climat <- climat[,-c(1,55)]


# design de la matrice de correlation
cormat <- round(cor(climat %>% select_if(is.numeric)),2)
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

lower_tri <- get_lower_tri(cormat)
melted_cormat <- melt(lower_tri, na.rm = TRUE)
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white")+scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,limit=c(-1,1),space="Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,size = 10,hjust = 1)) +
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
cormat[which(is.na(cormat[]))] <- 0

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

# Recuperer variables avec plus de 10% de NA pour les enlever
idx_bad_col <- which(apply(climat,2,function(x){sum(is.na(x))})> (dim(climat)[1]/10))
# et les variables avec variance trop faible, probleme pour l'acp sinon
near_zero_var <- nzv(climat, freqCut = 90/10)
# et les enlever
climat_rm_na <- climat[,-c(idx_bad_col,near_zero_var)]

#
#idx_bad_row <- which(apply(climat_rm_na,1,function(x){sum(is.na(x))}) > (dim(climat_rm_na)[2]/10))
#climat_rm_na <- climat_rm_na[-idx_bad_row,]

#remplace NA par 0 dans cormat
cormat[which(is.na(cormat[]))] <- 0

#fonction qui indique quel variables enlever
vire_var = findCorrelation(cormat, cutoff = 0.6, verbose = FALSE, names = FALSE, exact = T)

climat_rm_na_vire = climat_rm_na[,-vire_var]
preproc <- climat_rm_na_vire %>% select_if(is.numeric)
nb <- estim_ncpPCA(preproc,scale=T)
preproc_impute <- imputePCA(preproc,ncp=nb$ncp,scale=T)
