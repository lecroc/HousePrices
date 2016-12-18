### Model House Prices 5A more models

# Load libraries
library(MASS)
library(caret)
library(caretEnsemble)
library(quantregForest)
library(qrnn)
library(xgboost)
library(data.table)
library(Matrix)
library(methods)
library(vcd)
library(e1071)
library(DMwR)
library(mboost)
library(randomForest)
library(earth)
library(elasticnet)
library(arm)
library(rpart)
library(Cubist)
library(kernlab)
library(LiblineaR)
library(beepr)
library(plyr)
library(pls)
library(lars)
library(monomvn)

seed<-1234

# Get data

training<-read.csv("C:/Kaggle/HousePrices/train3.csv")
testing<-read.csv("C:/Kaggle/HousePrices/test3.csv")

## Create train and validation data sets from training data
set.seed (seed)
inTrain<-createDataPartition(y=training$SalePrice, p=.85, list=F)
HPtrn<-training[inTrain,]
HPtst<-training[-inTrain,]

PreObj<-preProcess(HPtrn[,2:250], method = c("nzv", "center", "scale"))
trntrans<-predict(PreObj, HPtrn[, 2:250])
HPtrn<-as.data.frame(cbind(SalePrice=log(HPtrn$SalePrice), trntrans))
trntsttrans<-predict(PreObj, HPtst[, 2:250])
HPtst<-as.data.frame(cbind(SalePrice=log(HPtst$SalePrice), trntsttrans))
testing<-predict(PreObj, testing)
HPtrn<-data.table(HPtrn, keep.rownames = F)
HPtst<-data.table(HPtst, keep.rownames = F)
testing<-data.table(testing, keep.rownames = F)


# Stacked model ensemble

# Set up training control
fitControl<-trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = "final")

# Test models

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(19, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

set.seed(seed)

m17<-train(SalePrice~., data=HPtrn, method="xgbTree", trControl=fitControl, metric="RMSE")
m17trnpred<-predict(m17, HPtrn)
TrnRMSE17<-sqrt(mean((HPtrn$SalePrice-m17trnpred)^2))
m17tstpred<-predict(m17, HPtst)
TstRMSE17<-sqrt(mean((HPtst$SalePrice-m17tstpred)^2))
save(m17, file="C:/Kaggle/HousePrices/m17.RData")

set.seed(seed)

m18<-train(SalePrice~., data=HPtrn, method="xgbLinear", trControl=fitControl, metric="RMSE")
m18trnpred<-predict(m18, HPtrn)
TrnRMSE18<-sqrt(mean((HPtrn$SalePrice-m18trnpred)^2))
m18tstpred<-predict(m18, HPtst)
TstRMSE18<-sqrt(mean((HPtst$SalePrice-m18tstpred)^2))
save(m18, file="C:/Kaggle/HousePrices/m18.RData")

set.seed(seed)

m19<-train(SalePrice~., data=HPtrn, method="bagEarthGCV", trControl=fitControl, metric="RMSE")
m19trnpred<-predict(m19, HPtrn)
TrnRMSE19<-sqrt(mean((HPtrn$SalePrice-m19trnpred)^2))
m19tstpred<-predict(m19, HPtst)
TstRMSE19<-sqrt(mean((HPtst$SalePrice-m19tstpred)^2))
save(m19, file="C:/Kaggle/HousePrices/m19.RData")

set.seed(seed)

m20<-train(SalePrice~., data=HPtrn, method="svmLinear3", trControl=fitControl, metric="RMSE")
m20trnpred<-predict(m20, HPtrn)
TrnRMSE20<-sqrt(mean((HPtrn$SalePrice-m20trnpred)^2))
m20tstpred<-predict(m20, HPtst)
TstRMSE20<-sqrt(mean((HPtst$SalePrice-m20tstpred)^2))
save(m20, file="C:/Kaggle/HousePrices/m20.RData")

set.seed(seed)

m21<-train(SalePrice~., data=HPtrn, method="cubist", trControl=fitControl, metric="RMSE")
m21trnpred<-predict(m21, HPtrn)
TrnRMSE21<-sqrt(mean((HPtrn$SalePrice-m21trnpred)^2))
m21tstpred<-predict(m21, HPtst)
TstRMSE21<-sqrt(mean((HPtst$SalePrice-m21tstpred)^2))
save(m21, file="C:/Kaggle/HousePrices/m21.RData")

set.seed(seed)

m22<-train(SalePrice~., data=HPtrn, method="bstTree", trControl=fitControl, metric="RMSE")
m22trnpred<-predict(m22, HPtrn)
TrnRMSE22<-sqrt(mean((HPtrn$SalePrice-m22trnpred)^2))
m22tstpred<-predict(m22, HPtst)
TstRMSE22<-sqrt(mean((HPtst$SalePrice-m22tstpred)^2))
save(m22, file="C:/Kaggle/HousePrices/m22.RData")

set.seed(seed)

m23<-train(SalePrice~., data=HPtrn, method="parRF", trControl=fitControl, metric="RMSE")
m23trnpred<-predict(m23, HPtrn)
TrnRMSE23<-sqrt(mean((HPtrn$SalePrice-m23trnpred)^2))
m23tstpred<-predict(m23, HPtst)
TstRMSE23<-sqrt(mean((HPtst$SalePrice-m23tstpred)^2))

set.seed(seed)

m24<-train(SalePrice~., data=HPtrn, method="glm", trControl=fitControl, metric="RMSE")
m24trnpred<-predict(m24, HPtrn)
TrnRMSE24<-sqrt(mean((HPtrn$SalePrice-m24trnpred)^2))
m24tstpred<-predict(m24, HPtst)
TstRMSE24<-sqrt(mean((HPtst$SalePrice-m24tstpred)^2))
save(m24, file="C:/Kaggle/HousePrices/m24.RData")

set.seed(seed)

m25<-train(SalePrice~., data=HPtrn, method="kernelpls", trControl=fitControl, metric="RMSE")
m25trnpred<-predict(m25, HPtrn)
TrnRMSE25<-sqrt(mean((HPtrn$SalePrice-m25trnpred)^2))
m25tstpred<-predict(m25, HPtst)
TstRMSE25<-sqrt(mean((HPtst$SalePrice-m25tstpred)^2))
save(m25, file="C:/Kaggle/HousePrices/m25.RData")

set.seed(seed)

m26<-train(SalePrice~., data=HPtrn, method="lars", trControl=fitControl, metric="RMSE")
m26trnpred<-predict(m26, HPtrn)
TrnRMSE26<-sqrt(mean((HPtrn$SalePrice-m26trnpred)^2))
m26tstpred<-predict(m26, HPtst)
TstRMSE26<-sqrt(mean((HPtst$SalePrice-m26tstpred)^2))
save(m26, file="C:/Kaggle/HousePrices/m26.RData")

set.seed(seed)

m27<-train(SalePrice~., data=HPtrn, method="glmboost", trControl=fitControl, metric="RMSE")
m27trnpred<-predict(m27, HPtrn)
TrnRMSE27<-sqrt(mean((HPtrn$SalePrice-m27trnpred)^2))
m27tstpred<-predict(m27, HPtst)
TstRMSE27<-sqrt(mean((HPtst$SalePrice-m27tstpred)^2))
save(m27, file="C:/Kaggle/HousePrices/m27.RData")

set.seed(seed)

m28<-train(SalePrice~., data=HPtrn, method="gbm", trControl=fitControl, metric="RMSE")
m28trnpred<-predict(m28, HPtrn)
TrnRMSE28<-sqrt(mean((HPtrn$SalePrice-m28trnpred)^2))
m28tstpred<-predict(m28, HPtst)
TstRMSE28<-sqrt(mean((HPtst$SalePrice-m28tstpred)^2))
save(m28, file="C:/Kaggle/HousePrices/m28.RData")

m29<-train(SalePrice~., data=HPtrn, method="lm", Control=fitControl, metric="RMSE")
m29trnpred<-predict(m29, HPtrn)
TrnRMSE29<-sqrt(mean((HPtrn$SalePrice-m29trnpred)^2))
m29tstpred<-predict(m29, HPtst)
TstRMSE29<-sqrt(mean((HPtst$SalePrice-m29tstpred)^2))
save(m29, file="C:/Kaggle/HousePrices/m29.RData")


modellist<-c("xgbTree", "xgbLinear", "bagEarthGCV", "svmLinear3",
             "cubist", "bstTree", "parRF", "glm", "kernelpls",
             "lars", "glmboost", "gbm", "lm")
             

Trnresults<-as.data.frame(rbind(TrnRMSE17, TrnRMSE18, TrnRMSE19,
                             TrnRMSE20, TrnRMSE21, TrnRMSE22, TrnRMSE23,
                             TrnRMSE24, TrnRMSE25, TrnRMSE26, TrnRMSE27,
                             TrnRMSE28, TrnRMSE29))
Tstresults<-as.data.frame(rbind(TstRMSE17, TstRMSE18, TstRMSE19,
                                TstRMSE20, TstRMSE21, TstRMSE22, TstRMSE23,
                                TstRMSE24, TstRMSE25, TstRMSE26, TstRMSE27,
                                TstRMSE28, TstRMSE29))

Results<-cbind(Trnresults, Tstresults)

row.names(Results)<-modellist
names(Results)<-c("TrainRMSE", "TestRMSE")

Results



# brnnn stack model

set.seed(seed)
stackbrnn <- train(SalePricemethod="brnn", metric="RMSE", trControl=fitControl)
print(stackbrnn)

stbpred<-predict(stackbrnn, testing)

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")

stksub<-as.data.frame(cbind(Id, SalePrice=exp(stbpred)))

stksub$SalePrice<-round(stksub$SalePrice, -2)


# check names / str of dfs

write.csv(stksub, "C:/Kaggle/HousePrices/stksubbrnn.csv", row.names = F)
