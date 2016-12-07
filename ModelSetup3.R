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

# Get data

training<-read.csv("C:/Kaggle/HousePrices/train3.csv")
testing<-read.csv("C:/Kaggle/HousePrices/test3.csv")

## Create train and validation data sets from training data
set.seed (1234)
inTrain<-createDataPartition(y=training$SalePrice, p=.85, list=F)
HPtrn<-training[inTrain,]
HPtst<-training[-inTrain,]

PreObj<-preProcess(HPtrn[,2:250], method = c("zv", "center", "scale", "pca"))
trntrans<-predict(PreObj, HPtrn[, 2:250])
HPtrn<-as.data.frame(cbind(SalePrice=log(HPtrn$SalePrice), trntrans))
trntsttrans<-predict(PreObj, HPtst[, 2:250])
HPtst<-as.data.frame(cbind(SalePrice=log(HPtst$SalePrice), trntsttrans))
testing<-predict(PreObj, testing)

# model 9

# Stepwise Regression (AIC)
 fit <- lm(SalePrice~.,data=HPtrn)
 step <- stepAIC(fit, direction="both")
 step$anova # display results

m9<-lm(SalePrice ~ PC1 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC10 + 
         PC14 + PC15 + PC16 + PC17 + PC18 + PC20 + PC22 + PC23 + PC24 + 
         PC25 + PC28 + PC29 + PC31 + PC33 + PC34 + PC35 + PC36 + PC37 + 
         PC38 + PC39 + PC40 + PC41 + PC42 + PC45 + PC47 + PC48 + PC49 + 
         PC50 + PC52 + PC54 + PC59 + PC61 + PC62 + PC64 + PC66 + PC68 + 
         PC70 + PC72 + PC75 + PC81 + PC83 + PC84 + PC91 + PC95 + PC97 + 
         PC99 + PC100 + PC102 + PC104 + PC105 + PC109 + PC110 + PC111 + 
         PC115 + PC116 + PC117 + PC118 + PC121 + PC122 + PC124 + PC126 + 
         PC127 + PC130 + PC134 + PC143 + PC146 + PC148 + PC149, data=HPtrn)

TrnRMSE9<-sqrt(mean((HPtrn$SalePrice-m9$fitted.values)^2))
m9tstpred<-predict(m9, HPtst)
TstRMSE9<-sqrt(mean((HPtst$SalePrice-m9tstpred)^2))

# Train RMSE
TrnRMSE9

# Test RMSE
TstRMSE9


#### gbm model ###

fitControl<-trainControl(method="repeatedcv", number=10, repeats=4)

gbmGrid<-expand.grid(interaction.depth=c(1,5,9), n.trees = (1:30)*50, shrinkage = c(.1, .05, .001), n.minobsinnode=c(6, 8, 10))

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

# Model 10 gbm

set.seed(2345)

m10<-train(SalePrice ~ ., data=HPtrn, method="gbm", trControl=fitControl,
          verbose=F, tuneGrid=gbmGrid, metric="RMSE")

m10trnpred<-predict(m10, HPtrn)
TrnRMSE10<-sqrt(mean((HPtrn$SalePrice-m10trnpred)^2))
m10tstpred<-predict(m10, HPtst)
TstRMSE10<-sqrt(mean((HPtst$SalePrice-m10tstpred)^2))


# Train RMSE
TrnRMSE10

# Test RMSE
TstRMSE10


### Model 11 Random Forest

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

# Model 11 Random Forest

set.seed(3456)

m11<- train(SalePrice ~ ., data=HPtrn, method="rf", trControl=fitControl,
           verbose=F, metric="RMSE")

m11trnpred<-predict(m11, HPtrn)
TrnRMSE11<-sqrt(mean((HPtrn$SalePrice-m11trnpred)^2))
m11tstpred<-predict(m11, HPtst)
TstRMSE11<-sqrt(mean((HPtst$SalePrice-m11tstpred)^2))

# Train RMSE
TrnRMSE11

# Test RMSE
TstRMSE11

### Model 12 MARS


## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

# Model 12 Multivariate Adaptive Regression Spline (MARS)

set.seed(4567)

m12<- train(SalePrice ~ ., data=HPtrn, method="earth", 
           trControl=fitControl, metric="RMSE", tuneLength=5)


m12trnpred<-predict(m12, HPtrn)
TrnRMSE12<-sqrt(mean((HPtrn$SalePrice-m12trnpred)^2))
m12tstpred<-predict(m12, HPtst)
TstRMSE12<-sqrt(mean((HPtst$SalePrice-m12tstpred)^2))

# Train RMSE
TrnRMSE12

# Test RMSE
TstRMSE12

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
pr9<-predict(m9, testing)
pr10<-predict(m10, testing)
pr11<-predict(m11, testing)
pr12<-predict(m12, testing)

s9<-as.data.frame(cbind(Id, Saleprice=exp(pr9)))
s10<-as.data.frame(cbind(Id, SalePrice=exp(pr10)))
s11<-as.data.frame(cbind(Id, SalePrice=exp(pr11)))
s12<-as.data.frame(cbind(Id, SalePrice=exp(pr12)))
names(s12)<-c("Id", "SalePrice")

write.csv(s9, "C:/Kaggle/HousePrices/s9.csv", row.names = F)
write.csv(s10, "C:/Kaggle/HousePrices/s10.csv", row.names = F)
write.csv(s11, "C:/Kaggle/HousePrices/s11.csv", row.names = F)
write.csv(s12, "C:/Kaggle/HousePrices/s12.csv", row.names = F)



