###############Import Data
Class = read.csv("D_class.csv", header = TRUE)
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

ReAP2D = cbind(DimFeat[2], ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)
