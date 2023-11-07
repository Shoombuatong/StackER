#########################################################
Grid <- expand.grid(.k = c(1, 3, 5, 7, 9))
prediction <- data.frame()
testsetCopy <- data.frame()
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "knn", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}
Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "knn", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mKNN = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################
Grid <- expand.grid(.lambda = c(0.0001, 0.001, 0.01), .alpha = 1)
prediction <- data.frame()
testsetCopy <- data.frame()
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "glmnet", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}
Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "glmnet", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mGLM = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "pls", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "pls", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mPLS = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "rpart", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "rpart", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mrpart = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "mlp", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "mlp", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mMLP = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train, method = "svmRadial", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D, method = "svmRadial", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mSVM = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################
Grid <- expand.grid (nrounds = c(20, 50, 100, 150, 200),
                       max_depth = c(3, 5, 7,  9, 10),
                       eta = c(0.1, 0.2, 0.3), gamma = 0, colsample_bytree = .7, min_child_weight = 5, subsample = 0.5)
prediction <- data.frame()
testsetCopy <- data.frame()
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "xgbTree", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}
Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "xgbTree", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mXGB = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

######################################################
Grid <- expand.grid(.mtry=c(3, 5, 7,  9, 10), .ntree=c(20, 50, 100, 200, 300))
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method=customRF,  trControl = cctrl, tuneGrid= Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D, method=customRF,  trControl = cctrl, tuneGrid= Grid, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mRF = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

########################################################
