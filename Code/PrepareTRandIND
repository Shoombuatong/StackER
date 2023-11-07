###############Import Data
Class = read.csv("D_class.csv", header = TRUE)
Cdes = read.csv("AP2D.csv", header = TRUE)
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
