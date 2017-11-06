library(caret)
library(pls)

idx_test <- c(7110,4149,7222,7481,7535,7591,7630,7747,7790)

### PCR ###

test <- preproc[preproc$NUM_POSTE %in% idx_test,]
test <- test[,-which(colnames(test) %in% c("NUM_POSTE","FXAB"))]
training_pca <- preproc[-(preproc$NUM_POSTE %in% idx_test),]
training_pca <- training_pca[,-which(colnames(training_pca) %in% c("NUM_POSTE","FXAB"))]

res_pca <- PCA(training_pca, ncp = dim(training_pca)[2], scale.unit = TRUE, graph=F)

train_proj <- as.data.frame(res_pca$ind$coord)
test_standard <- t(apply(test,1,function(x){(x-res_pca$call$centre)/res_pca$call$ecart.type} ))
test_proj <- as.data.frame(as.matrix(test_standard) %*% res_pca$svd$V)
train_proj$FXAB<- preproc$FXAB[-(preproc$NUM_POSTE %in% idx_test)]
test_proj$FXAB <- preproc$FXAB[preproc$NUM_POSTE %in% idx_test]
colnames(test_proj) <- colnames(train_proj)

pcr_fit <- pcr(train_proj$FXAB ~ ., data = train_proj, scale = F, validation = "CV")
pred <- predict(pcr_fit, ncomp = dim(test_proj)[2]-1, newdata = test_proj) %>% as.data.frame %>% pull()
plot(pred,test_proj$FXAB)
abline(a=0,b=1,col="red")
RMSE(pred,test_proj$FXAB)



### Random Forest ###

test <- preproc[preproc$NUM_POSTE %in% idx_test,]
training <- preproc[-(preproc$NUM_POSTE %in% idx_test),]

control <- trainControl(method="repeatedcv", number=10, repeats=3)

mtry <- sqrt(ncol(training))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(FXAB~., data=training, method="rf", tuneGrid=tunegrid, trControl=control)
RMSE(predict(rf_default,test),test$FXAB)