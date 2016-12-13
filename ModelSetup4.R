### Model House Prices 4

# Load libraries
library(MASS)
library(caret)
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

# Get data

training<-read.csv("C:/Kaggle/HousePrices/train3.csv")
testing<-read.csv("C:/Kaggle/HousePrices/test3.csv")

## Create train and validation data sets from training data
set.seed (1234)
inTrain<-createDataPartition(y=training$SalePrice, p=.85, list=F)
HPtrn<-training[inTrain,]
HPtst<-training[-inTrain,]

PreObj<-preProcess(HPtrn[,2:250], method = c("zv", "center", "scale"))
trntrans<-predict(PreObj, HPtrn[, 2:250])
HPtrn<-as.data.frame(cbind(SalePrice=log(HPtrn$SalePrice), trntrans))
trntsttrans<-predict(PreObj, HPtst[, 2:250])
HPtst<-as.data.frame(cbind(SalePrice=log(HPtst$SalePrice), trntsttrans))
testing<-predict(PreObj, testing)


## Model 13 xgBoost

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

fitControl<-trainControl(method="cv", number=10)

trdf<- data.table(HPtrn, keep.rownames=F)
tedf<-data.table(HPtst, keep.rownames = F)

set.seed(4321)

m13 <-train(SalePrice ~., data=trdf, verbose=T, method="xgbTree", metric = "RMSE", trControl=fitControl, tuneLength=5)

m13trnpred<-predict(m13, HPtrn)
TrnRMSE13<-sqrt(mean((HPtrn$SalePrice-m13trnpred)^2))
m13tstpred<-predict(m13, HPtst)
TstRMSE13<-sqrt(mean((HPtst$SalePrice-m13tstpred)^2))

# Train RMSE
TrnRMSE13

# Test RMSE
TstRMSE13


## Model 14 ridge

fitControl<-trainControl(method="cv", number=10)

ridgeGrid<-expand.grid(lambda=c(.01, .1, 1, 10))

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

set.seed(2345)

m14<-train(SalePrice ~ ., data=HPtrn, method="ridge", trControl=fitControl, verbose=T, tuneGrid=ridgeGrid, metric="RMSE")

m14trnpred<-predict(m14, HPtrn)
TrnRMSE14<-sqrt(mean((HPtrn$SalePrice-m14trnpred)^2))
m14tstpred<-predict(m14, HPtst)
TstRMSE14<-sqrt(mean((HPtst$SalePrice-m14tstpred)^2))


# Train RMSE
TrnRMSE14

# Test RMSE
TstRMSE14


## Model 15 bagged MARS

fitControl<-trainControl(method="cv", number=4)

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

set.seed(2345)

m15<-train(SalePrice ~ ., data=HPtrn, method="bagEarthGCV", metric="RMSE")

m15trnpred<-predict(m15, HPtrn)
TrnRMSE15<-sqrt(mean((HPtrn$SalePrice-m15trnpred)^2))
m15tstpred<-predict(m15, HPtst)
TstRMSE15<-sqrt(mean((HPtst$SalePrice-m15tstpred)^2))


# Train RMSE
TrnRMSE15

# Test RMSE
TstRMSE15


## Model 16 bayes glm

fitControl<-trainControl(method="cv", number=10)

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

set.seed(2345)

m16<-train(SalePrice ~ ., data=HPtrn, method="bayesglm", trControl=fitControl, tuneLength=5, metric="RMSE")

m16trnpred<-predict(m16, HPtrn)
TrnRMSE16<-sqrt(mean((HPtrn$SalePrice-m16trnpred)^2))
m16tstpred<-predict(m16, HPtst)
TstRMSE16<-sqrt(mean((HPtst$SalePrice-m16tstpred)^2))


# Train RMSE
TrnRMSE16

# Test RMSE
TstRMSE16




Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
pr13<-predict(m13, testing)
pr14<-predict(m14, testing)
pr15<-predict(m15, testing)
pr16<-predict(m16, testing)

s13<-as.data.frame(cbind(Id, SalePrice=exp(pr13)))
s14<-as.data.frame(cbind(Id, SalePrice=exp(pr14)))
s15<-as.data.frame(cbind(Id, SalePrice=exp(pr15)))
s16<-as.data.frame(cbind(Id, SalePrice=exp(pr16)))

s13$SalePrice<-round(s13$SalePrice, -2)
s14$SalePrice<-round(s14$SalePrice, -2)
s15$SalePrice<-round(s15$SalePrice, -2)
s16$SalePrice<-round(s16$SalePrice, -2)


# check names / str of dfs

write.csv(s13, "C:/Kaggle/HousePrices/s13.csv", row.names = F)
write.csv(s14, "C:/Kaggle/HousePrices/s14.csv", row.names = F)
write.csv(s15, "C:/Kaggle/HousePrices/s15.csv", row.names = F)
write.csv(s16, "C:/Kaggle/HousePrices/s16.csv", row.names = F)



