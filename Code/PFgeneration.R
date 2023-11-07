PF1 = matrix(nrow = nrow(Dtr), ncol = 10)
PF2 = matrix(nrow = nrow(Dts), ncol = 10)
PFcv = matrix(nrow = nrow(Dtr), ncol = 1)
PFind = matrix(nrow = nrow(Dts), ncol = 1)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = test,  method = ML method, trControl = cctrl, tuneLength = 10, metric=c("Accuracy"), na.action=na.exclude)
PF1[, h] <- predict(M, Dtr, type="prob", se.fit=TRUE)[,1]
PF2[, h] <- predict(M, Dts, type="prob", se.fit=TRUE)[,1]
}

for (i in 1: nrow(Dtr)){
PFcv[i,] = mean(PF1[i,])
}

for (i in 1: nrow(Dts)){
PFind[i,] = mean(PF2[i,])
}

PFV1 = rbind(data.frame(P = PFcv), data.frame(P = PFind))
