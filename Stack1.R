### Model House Prices 2

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


# Models from data setup 1

train1<-read.csv("C:/Kaggle/HousePrices/train1.csv")
test1<-read.csv("C:/Kaggle/HousePrices/test1.csv")
load("C:/Kaggle/HousePrices/m1.RData")
load("C:/Kaggle/HousePrices/m2.RData")

trp1<-predict(m1, train1)
trp2<-predict(m2, train1)

tep1<-predict(m1, test1)
tep2<-predict(m2, test1)



# Models from data setup 2

train2<-read.csv("C:/Kaggle/HousePrices/train2.csv")
test2<-read.csv("C:/Kaggle/HousePrices/test2.csv")
load("C:/Kaggle/HousePrices/m5.RData")
load("C:/Kaggle/HousePrices/m6.RData")
load("C:/Kaggle/HousePrices/m8.RData")

trp3<-predict(m5, train2)
trp4<-predict(m6, train2)
trp5<-predict(m8, train2)

tep3<-predict(m5, test2)
tep4<-predict(m6, test2)
tep5<-predict(m8, test2)


# Models from data setup 3

train3<-read.csv("C:/Kaggle/HousePrices/train3.csv")
test3<-read.csv("C:/Kaggle/HousePrices/test3.csv")
PreObj<-preProcess(train3[,2:250], method = c("zv", "center", "scale"))
t3<-predict(PreObj, train3[, 2:250])
train3<-as.data.frame(cbind(SalePrice=log(train3$SalePrice), t3))
test3<-predict(PreObj, test3)


load("C:/Kaggle/HousePrices/m9.RData")
load("C:/Kaggle/HousePrices/m10.RData")
load("C:/Kaggle/HousePrices/m12.RData")

trp6<-predict(m9, train3)
trp7<-predict(m10, train3)
trp8<-predict(m12, train3)

tep6<-predict(m9, test3)
tep7<-predict(m10, test3)
tep8<-predict(m12, test3)

SalePrice<-train2$SalePrice
StackTrain<-as.data.frame(cbind(SalePrice, trp1, trp2, trp3, trp4, trp5, trp6, trp7, trp8))
names(StackTrain)<-c("SalePrice", "trp1", "trp2", "trp3", "trp4", "trp5", "trp6", "trp7", "trp8")

### Stack Model 

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

# Stack Model nnet

set.seed(2345)

StkM<-train(SalePrice ~ ., data=StackTrain, method="brnn")

StkMpred<-predict(StkM, StackTrain)
StkRMSE<-sqrt(mean((StackTrain$SalePrice-StkMpred)^2))
StkRMSE

StackTest<-as.data.frame(cbind(tep1, tep2, tep3, tep4, tep5, tep6, tep7, tep8))
names(StackTest)<-c("trp1", "trp2", "trp3", "trp4", "trp5", "trp6", "trp7", "trp8")

StkTestPred<-predict(StkM, StackTest)

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
StkTestPred<-exp(StkTestPred)
StkTestPred<-round(StkTestPred, -2)
StkSub<-as.data.frame(cbind(Id, Saleprice=StkTestPred))
write.csv(StkSub, file="C:/Kaggle/HousePrices/StkSub.csv", row.names = F)
