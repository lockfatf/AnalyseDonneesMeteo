library(caret)
library(pls)
library(reshape2)


# Chargement des donnees avec fread, plus intelligent que read.csv
climat <- fread("Données/climat.201708.csv",data.table = F)
villes <- fread("Données/postesSynop.csv",data.table=F)
climat <- merge(climat,villes,by="NUM_POSTE",all.x = T)
climat$Nom[is.na(climat$Nom)] <- "Nom Inconnu"
row.names(climat) <- climat$Nom
climat <- climat[-which(is.na(climat$FXAB)),]

idx_test <- c(7110,7149,7222,7481,7535,7591,7630,7747,7790)
test_villes <- rownames(climat)[climat$NUM_POSTE %in% idx_test]

training <- climat[-which(climat$NUM_POSTE %in% idx_test),-which(colnames(climat) %in% c("NUM_POSTE","FXAB"))]
test <- climat[climat$NUM_POSTE %in% idx_test,-which(colnames(climat) %in% c("NUM_POSTE","FXAB"))]

training <- training %>% select_if(is.numeric)

idx_bad_col <- which(apply(training,2,function(x){sum(is.na(x))})> (dim(training)[1]/20))
training <- training[,-idx_bad_col]
training_bagImpute <- preProcess(training,method=c("bagImpute","nzv"))
training <- predict(training_bagImpute,training)
test <- test[,which(colnames(test) %in% colnames(training))]
test <- predict(training_bagImpute,test)

# theme1 <- trellis.par.get()
# theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
# theme1$plot.symbol$pch = 16
# theme1$plot.line$col = rgb(1, 0, 0, .7)
# theme1$plot.line$lwd <- 2
# trellis.par.set(theme1)
# featurePlot(x = training, 
            # y = climat$FXAB[-which(climat$NUM_POSTE %in% idx_test)], 
            # plot = "scatter",
            # type = c("p", "smooth"),
            # span = .5,
            # layout = c(10, 3))

### PCR ###

res_pca_explo <- PCA(training, ncp = dim(training)[2], scale.unit = TRUE, graph=F)
contrib_explo <- fviz_contrib(res_pca_explo,axes=c(1:8),choice="ind")
high_contrib_names <- as.character(contrib_explo$data$name[contrib_explo$data$contrib > 2])

training_pca <- training[-which(rownames(training) %in% high_contrib_names),]

par(mfrow=c(1,2))
res_pca <- PCA(training_pca, ncp = dim(training_pca)[2], scale.unit = TRUE, graph=T)
train_proj <- as.data.frame(res_pca$ind$coord)
test_standard <- t(apply(test,1,function(x){(x-res_pca$call$centre)/res_pca$call$ecart.type} ))
test_proj <- as.data.frame(as.matrix(test_standard) %*% res_pca$svd$V)
train_proj$FXAB<- climat$FXAB[which(rownames(climat) %in% rownames(train_proj))]
test_proj$FXAB <- climat$FXAB[which(rownames(climat) %in% rownames(test_proj))]
colnames(test_proj) <- colnames(train_proj)

set.seed(123)
pcr_fit <- pcr(FXAB ~ ., data = train_proj, scale = F, validation = "CV")
par(mfrow=c(1,1))
plot(RMSEP(pcr_fit), legendpos = "topleft")
ncomp.min <- which.min(sapply(1:dim(pcr_fit$validation$pred)[3],function(x){
  RMSE(pcr_fit$validation$pred[,,x],train_proj$FXAB)
}))
pcr_fit <- pcr(FXAB ~ ., data = train_proj,ncomp=ncomp.min, scale = F, validation = "CV")
pred <- predict(pcr_fit, ncp = ncomp.min, newdata = test_proj) %>% as.data.frame %>% pull()
plot(pred,test_proj$FXAB)
abline(a=0,b=1,col="red")
RMSE(pred,test_proj$FXAB)
sqrt(sum((test_proj$FXAB - pred)^2))

### PLSR ###

training_plsr <- training[-which(rownames(training) %in% high_contrib_names),]
training_plsr$FXAB <- climat$FXAB[which(rownames(climat) %in% rownames(training_plsr))]
test_plsr <- test
test_plsr$FXAB <- climat$FXAB[which(rownames(climat) %in% rownames(test_plsr))]

set.seed(123)
plsr_fit <- plsr(FXAB ~ ., data = training_plsr, scale = T,validation="CV")
plot(RMSEP(plsr_fit), legendpos = "topleft")
ncomp.min <- which.min(sapply(1:dim(plsr_fit$validation$pred)[3],function(x){
  RMSE(plsr_fit$validation$pred[,,x],training_plsr$FXAB)
}))
plsr_fit <- plsr(FXAB ~ ., data = training_plsr, ncomp=ncomp.min,scale = T,validation="CV")
pred_plsr <- predict(plsr_fit, ncp = ncomp.min, newdata = test_plsr) %>% as.data.frame %>% pull()
RMSE(pred_plsr,test_plsr$FXAB)
sqrt(sum((test_plsr$FXAB - pred_plsr)^2))

### Random Forest sur preproc apres transfo pca ###

rf_default <- train(FXAB~., data=training_plsr, method="rf")
RMSE(predict(rf_default,test_plsr),test_plsr$FXAB)

### Random Forest sur preproc ###

library(doParallel)
registerDoParallel(4)

training$FXAB <- climat$FXAB[-which(climat$NUM_POSTE %in% idx_test)]
test$FXAB <- climat$FXAB[which(climat$NUM_POSTE %in% idx_test)]


#control <- trainControl(method="boot")
#mtry <- sqrt(ncol(training))
#tunegrid <- expand.grid(.mtry=c(1:(dim(training)[2]-1)))
set.seed(123)
mod_rborist <- train(FXAB~., data=training, method="Rborist")
mod_svm <- train(FXAB~., data=training, method="svmLinear2")
mod_glmnet <- train(FXAB~., data=training, method="glmnet")
RMSE(predict(mod_rborist,test),test$FXAB)
RMSE(predict(mod_svm,test),test$FXAB)
RMSE(predict(mod_glmnet,test),test$FXAB)

#sqrt(sum((test$FXAB - predict(rf_default,test))^2))
#plot(predict(rf_default,test),test$FXAB)
#abline(a=0,b=1,col="blue")

df_pred <- data.frame(predict(mod_rborist,test),predict(mod_svm,test),predict(mod_glmnet,test),test$FXAB)
colnames(df_pred) <- c("Rborist","svm","glmnet","Observations")
df_pred$Villes <- rownames(test)
melt_preds <- melt(df_pred,id=c("Observations","Villes"))
ggplot(melt_preds,aes(Observations,value,colour=variable))+geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method="lm",se=F)

df_pred_train <- data.frame(predict(mod_rborist,training),predict(mod_svm,training),predict(mod_glmnet,training),training$FXAB)
colnames(df_pred_train) <- c("Rborist","svm","glmnet","Observations")
df_pred_train$Villes <- rownames(df_pred_train)
melt_preds_train <- melt(df_pred_train,id=c("Observations","Villes"))
ggplot(melt_preds_train,aes(Observations,value,colour=variable))+geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method="lm",se=F)

mod_ensemble <- train(Observations~.,data=df_pred_train[,-dim(df_pred_train)[2]],method="gam")
df_pred$ensemble <- predict(mod_ensemble,df_pred)
melt_preds <- melt(df_pred,id=c("Observations","Villes"))
ggplot(melt_preds,aes(Observations,value,colour=variable))+geom_point()+geom_abline(intercept=0,slope=1)+geom_smooth(method="lm",se = F)
RMSE(df_pred$ensemble,df_pred$Observations)
plot(df_pred$ensemble,df_pred$Observations)
abline(0,1)

res <- sapply(1:10,function(x){
  cat(x,"\n")
  inTrain <- createDataPartition(climat$FXAB,p=0.8,list=F)
  training <- climat[inTrain,-which(colnames(climat) %in% c("NUM_POSTE","FXAB"))]
  test <- climat[-inTrain,-which(colnames(climat) %in% c("NUM_POSTE","FXAB"))]
  
  training <- training %>% select_if(is.numeric)
  idx_bad_col <- which(apply(training,2,function(x){sum(is.na(x))})> (dim(training)[1]/2))
  training <- training[,-idx_bad_col]
  training_bagImpute <- preProcess(training,method=c("bagImpute","nzv"))
  training <- predict(training_bagImpute,training)
  test <- test[,which(colnames(test) %in% colnames(training))]
  test <- predict(training_bagImpute,test)
  
  training$FXAB <- climat$FXAB[inTrain]
  test$FXAB <- climat$FXAB[-inTrain]
  
  mod_rborist <- train(FXAB~., data=training, method="Rborist")
  mod_svm <- train(FXAB~., data=training, method="svmLinear2")
  mod_glmnet <- train(FXAB~., data=training, method="glmnet")

  df_pred <- data.frame(predict(mod_rborist,test),predict(mod_svm,test),predict(mod_glmnet,test),test$FXAB)
  colnames(df_pred) <- c("Rborist","svm","glmnet","Observations")
  df_pred$Villes <- rownames(test)

  df_pred_train <- data.frame(predict(mod_rborist,training),predict(mod_svm,training),predict(mod_glmnet,training),training$FXAB)
  colnames(df_pred_train) <- c("Rborist","svm","glmnet","Observations")
  df_pred_train$Villes <- rownames(df_pred_train)

  mod_ensemble <- train(Observations~.,data=df_pred_train[,-dim(df_pred_train)[2]],method="gam")
  df_pred$ensemble <- predict(mod_ensemble,df_pred)
  rmses <- c(RMSE(df_pred$Rborist,df_pred$Observations),RMSE(df_pred$svm,df_pred$Observations),RMSE(df_pred$glmnet,df_pred$Observations),RMSE(df_pred$ensemble,df_pred$Observations))
  names(rmses) <- c("Rborist","svm","glmnet","ensemble")
  return(rmses)
  
})
