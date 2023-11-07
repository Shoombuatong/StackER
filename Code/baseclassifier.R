###############Import Data
Class = read.csv("Dataset_alpha.csv", header = TRUE)
Cdes = read.csv("Descriptor.csv", header = TRUE)
k = 10
cctrl <-trainControl(method = "cv", number = 5, classProbs =  TRUE)

##############Remove high Cor
Dat = scale(Cdes[, -1])
Dat = remove_empty(Dat, which = c("cols"), cutoff = 1, quiet = TRUE)
datMy.scale<- Dat[ , ! apply( Dat , 2 , function(x) all(is.na(x)) ) ]
corMatMy <- cor(datMy.scale)
highlyCor <- findCorrelation(corMatMy, 0.99)
FeatLowCor <- datMy.scale[,-highlyCor]

#########################################################
D = data.frame(Dat, Activity = Class[,4])
active   <- subset(D, Activity == 'active')
inactive <- subset(D, Activity == 'inactive')
ntr1 <- floor(0.8 * nrow(active))
ntr2 <- floor(0.8 * nrow(inactive))
train_ind1 <- sample(seq_len(nrow(active)), size = ntr1)
train_ind2 <- sample(seq_len(nrow(inactive)), size = ntr2)
set.seed(123)
trpos <- active[train_ind1, ]
tspos <- active[-train_ind1, ]
trneg <- inactive[train_ind2, ]
tsneg <- inactive[-train_ind2, ]
Dtr = rbind(trpos, trneg)
Dts = rbind(tspos, tsneg)
DimFeat = dim(Dtr) - 1
id <- sample(1:k,nrow(Dtr),replace=TRUE)

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "pls", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr,  method = "pls", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

RePLS = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################
Grid <- expand.grid(.k = c(1, 3, 5, 7, 9))                           
prediction <- data.frame()
testsetCopy <- data.frame()

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "knn", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr,  method = "knn", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

ReKNN = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)
#########################################################
Grid <- expand.grid(.lambda = c(0.0001, 0.001, 0.01), .alpha = 1)
prediction <- data.frame()
testsetCopy <- data.frame()

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "glmnet", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr,  method = "glmnet", trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

ReGLM = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################
 prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "rpart", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr,  method = "rpart", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

Rerpart = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)     
#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "mlp", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr,  method = "mlp", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

ReMLP = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "svmRadial", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr,  method = "svmRadial", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

ReSVM = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################
Grid <- expand.grid(.mtry=c(5, 7, 10, 15), .ntree=c(20, 50, 100, 200, 300))
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method= customRF, trControl = cctrl, tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr,  method= customRF,  tuneGrid=Grid, trControl = cctrl, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

ReRF = cbind( ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)


#########################################################
Grid <- expand.grid (nrounds = c(20, 50, 100, 150, 200),
                       max_depth = c(3, 5, 7,  9, 10),
                       eta = c(0.1, 0.2, 0.3), gamma = 0, colsample_bytree = .7, min_child_weight = 5, subsample = 0.5)

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "xgbTree", trControl = cctrl,tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr,  method= "xgbTree",  tuneGrid=Grid, trControl = cctrl, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

ReXGB = cbind(ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)
