### Stacked Model for House Prices 

# Stacking 16 models created with different data setups and algorithms

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
library(plyr)
library(nnet)
library(brnn)
library(foreach)
library(elasticnet)
library(arm)


# Models from data setup 1

train1<-read.csv("C:/Kaggle/HousePrices/train1.csv")
test1<-read.csv("C:/Kaggle/HousePrices/test1.csv")
load("C:/Kaggle/HousePrices/m1.RData")
load("C:/Kaggle/HousePrices/m2.RData")
load("C:/Kaggle/HousePrices/m3.RData")
load("C:/Kaggle/HousePrices/m4.RData")

trp1<-predict(m1, train1) # Step AIC regression with outliers removed and interaction term for neighborhood:GrLivArea
trp2<-predict(m2, train1) # gbm with 10 fold repeated cv with 4 repeats
trp3<-predict(m3, train1) # rf model with oob with mtry (4, 16, 4), ntree=10,000
trp4<-predict(m4, train1) # MARS model 10 fold repeated cv with 4 repeats

tep1<-predict(m1, test1)
tep2<-predict(m2, test1)
tep3<-predict(m3, test1)
tep4<-predict(m4, test1)

# Models from data setup 2

train2<-read.csv("C:/Kaggle/HousePrices/train2.csv")
test2<-read.csv("C:/Kaggle/HousePrices/test2.csv")
load("C:/Kaggle/HousePrices/m5.RData")
load("C:/Kaggle/HousePrices/m6.RData")
load("C:/Kaggle/HousePrices/m7.RData")
load("C:/Kaggle/HousePrices/m8.RData")

trp5<-predict(m5, train2)
trp6<-predict(m6, train2)
trp7<-predict(m7, train2)
trp8<-predict(m8, train2)

tep5<-predict(m5, test2)
tep6<-predict(m6, test2)
tep7<-predict(m7, test2)
tep8<-predict(m8, test2)


# Models from data setup 3

train3<-read.csv("C:/Kaggle/HousePrices/train3.csv")
test3<-read.csv("C:/Kaggle/HousePrices/test3.csv")
PreObj<-preProcess(train3[,2:250], method = c("zv", "center", "scale"))
t3<-predict(PreObj, train3[, 2:250])
train3<-as.data.frame(cbind(SalePrice=log(train3$SalePrice), t3))
test3<-predict(PreObj, test3)

load("C:/Kaggle/HousePrices/m9.RData")
load("C:/Kaggle/HousePrices/m10.RData")
load("C:/Kaggle/HousePrices/m11.RData")
load("C:/Kaggle/HousePrices/m12.RData")



trp9<-predict(m9, train3)
trp10<-predict(m10, train3)
trp11<-predict(m11, train3)
trp12<-predict(m12, train3)

tep9<-predict(m9, test3)
tep10<-predict(m10, test3)
tep11<-predict(m11, test3)
tep12<-predict(m12, test3)

# More Models from data setup 3

load("C:/Kaggle/HousePrices/m13.RData")
load("C:/Kaggle/HousePrices/m14.RData")
load("C:/Kaggle/HousePrices/m15.RData")
load("C:/Kaggle/HousePrices/m16.RData")

trp13<-predict(m13, train3)
trp14<-predict(m14, train3)
trp15<-predict(m15, train3)
trp16<-predict(m16, train3)

tep13<-predict(m13, test3)
tep14<-predict(m14, test3)
tep15<-predict(m15, test3)
tep16<-predict(m16, test3)


SalePrice<-train2$SalePrice
StackTrain<-as.data.frame(cbind(SalePrice, trp1, trp5, trp6, trp8, trp9, trp10, trp12, trp15, trp16))
names(StackTrain)<-c("SalePrice", "trp1", "trp5", "trp6", "trp8", "trp9", "trp10", "trp12", "trp15", "trp16")

StackTest<-as.data.frame(cbind(tep1, tep5, tep6, tep8, tep9, tep10, tep12, tep15, tep16))
names(StackTest)<-c("trp1", "trp5", "trp6", "trp8", "trp9", "trp10", "trp12", "trp15", "trp16")


## Stacked Model xgBoost

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

fitControl<-trainControl(method="cv", number=10)

StackTrain<- data.table(StackTrain, keep.rownames=F)
StackTest<-data.table(StackTest, keep.rownames = F)

set.seed(9999)

StkM <-train(SalePrice ~., data=StackTrain, verbose=T, method="xgbTree", metric = "RMSE", trControl=fitControl, tuneLength=5)


StkMpred<-predict(StkM, StackTrain)
StkRMSE<-sqrt(mean((StackTrain$SalePrice-StkMpred)^2))
StkRMSE


StkTestPred<-predict(StkM, StackTest)

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
StkTestPred<-exp(StkTestPred)
StkTestPred<-round(StkTestPred, -2)
StkSub<-as.data.frame(cbind(Id, Saleprice=StkTestPred))
write.csv(StkSub, file="C:/Kaggle/HousePrices/StkSub1.csv", row.names = F)
