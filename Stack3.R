### Stacked Model for House Prices 

# Stacking 20 models created with different data setups and algorithms

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
library(gbm)
library(survival)
library(splines)
library(bst)
library(brnn)
library(Formula)



# Model from data setup 1

train1<-read.csv("C:/Kaggle/HousePrices/train1.csv")
test1<-read.csv("C:/Kaggle/HousePrices/test1.csv")
load("C:/Kaggle/HousePrices/m1.RData")

trp1<-predict(m1, train1) # Step AIC regression with outliers removed and interaction term for neighborhood:GrLivArea
tep1<-predict(m1, test1)


# Models from data setup 2

train2<-read.csv("C:/Kaggle/HousePrices/train2.csv")
test2<-read.csv("C:/Kaggle/HousePrices/test2.csv")
load("C:/Kaggle/HousePrices/m5.RData")
load("C:/Kaggle/HousePrices/m6.RData")
load("C:/Kaggle/HousePrices/m8.RData")

trp5<-predict(m5, train2)
trp6<-predict(m6, train2)
trp8<-predict(m8, train2)

tep5<-predict(m5, test2)
tep6<-predict(m6, test2)
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
load("C:/Kaggle/HousePrices/m12.RData")

trp9<-predict(m9, train3)
trp10<-predict(m10, train3)
trp12<-predict(m12, train3)

tep9<-predict(m9, test3)
tep10<-predict(m10, test3)
tep12<-predict(m12, test3)

# More Models from data setup 3

load("C:/Kaggle/HousePrices/m15.RData")
load("C:/Kaggle/HousePrices/m16.RData")

trp15<-predict(m15, train3)
trp16<-predict(m16, train3)

tep15<-predict(m15, test3)
tep16<-predict(m16, test3)

# Models from data setup 4

train4<-read.csv("C:/Kaggle/HousePrices/train3.csv")
test4<-read.csv("C:/Kaggle/HousePrices/test3.csv")
PreObj<-preProcess(train4[,2:250], method = c("nzv", "center", "scale"))
t4<-predict(PreObj, train4[, 2:250])
train4<-as.data.frame(cbind(SalePrice=log(train4$SalePrice), t4))
test4<-predict(PreObj, test4)

load("C:/Kaggle/HousePrices/m17.RData")
load("C:/Kaggle/HousePrices/m18.RData")
load("C:/Kaggle/HousePrices/m19.RData")
load("C:/Kaggle/HousePrices/m21.RData")
load("C:/Kaggle/HousePrices/m22.RData")
load("C:/Kaggle/HousePrices/m24.RData")
load("C:/Kaggle/HousePrices/m25.RData")
load("C:/Kaggle/HousePrices/m26.RData")
load("C:/Kaggle/HousePrices/m27.RData")
load("C:/Kaggle/HousePrices/m28.RData")
load("C:/Kaggle/HousePrices/m29.RData")

trp17<-predict(m17, train4)
trp18<-predict(m18, train4)
trp19<-predict(m19, train4)
trp21<-predict(m21, train4)
trp22<-predict(m22, train4)
trp24<-predict(m24, train4)
trp25<-predict(m25, train4)
trp26<-predict(m26, train4)
trp27<-predict(m27, train4)
trp28<-predict(m28, train4)
trp29<-predict(m29, train4)

tep17<-predict(m17, test4)
tep18<-predict(m18, test4)
tep19<-predict(m19, test4)
tep21<-predict(m21, test4)
tep22<-predict(m22, test4)
tep24<-predict(m24, test4)
tep25<-predict(m25, test4)
tep26<-predict(m26, test4)
tep27<-predict(m27, test4)
tep28<-predict(m28, test4)
tep29<-predict(m29, test4)


SalePrice<-train2$SalePrice
StackTrain<-as.data.frame(cbind(SalePrice, trp1, trp6, trp8, trp10, trp12, trp19, trp21, trp26))
names(StackTrain)<-c("SalePrice", "trp1", "trp6", "trp8", "trp10", "trp12", "trp19", "trp21", "trp26")

StackTest<-as.data.frame(cbind(tep1, tep6, tep8, tep10, tep12, tep19, tep21, tep26))

names(StackTest)<-c("trp1", "trp6", "trp8", "trp10", "trp12", "trp19", "trp21","trp26")


## Stacked Model brnn

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(19, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

fitControl<-trainControl(method="repeatedcv", number=10, repeats=3)

set.seed(9999)

StkM3 <-train(SalePrice ~., data=StackTrain, verbose=T, method="brnn", metric = "RMSE", trControl=fitControl, tuneLength=5)


StkMpred<-predict(StkM3, StackTrain)
StkRMSE<-sqrt(mean((StackTrain$SalePrice-StkMpred)^2))
StkRMSE


StkTestPred<-predict(StkM3, StackTest)

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
StkTestPred<-exp(StkTestPred)
StkTestPred<-round(StkTestPred, -2)
StkSub<-as.data.frame(cbind(Id, Saleprice=StkTestPred))
write.csv(StkSub, file="C:/Kaggle/HousePrices/StkSub3.csv", row.names = F)


## Just take an average and see.....

Avg<-(tep1+tep6+tep8+tep10+tep12+tep19+tep21+tep26)/8

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
Avg<-exp(Avg)
Avg<-round(Avg, -2)
AvgStkSub<-as.data.frame(cbind(Id, Saleprice=Avg))
names(AvgStkSub)<-c("Id", "SalePrice")
write.csv(AvgStkSub, file="C:/Kaggle/HousePrices/AvgStkSub.csv", row.names = F)
