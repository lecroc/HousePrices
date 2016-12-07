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

training<-read.csv("C:/Kaggle/HousePrices/train2.csv")
testing<-read.csv("C:/Kaggle/HousePrices/test2.csv")

## Create train and validation data sets from training data
set.seed (1234)
inTrain<-createDataPartition(y=training$SalePrice, p=.85, list=F)
HPtrn<-training[inTrain,]
HPtst<-training[-inTrain,]


# model 5

## initialize for parallel processing

# library(doSNOW)
# getDoParWorkers()
# registerDoSNOW(makeCluster(7, type="SOCK"))
# getDoParWorkers()
# getDoParName()
# library(foreach)

# Stepwise Regression (AIC)
# fit <- lm(SalePrice~.,data=HPtrn)
# step <- stepAIC(fit, direction="both")
# step$anova # display results

m5<-lm(SalePrice ~ MSSubClass.20 + MSSubClass.30 + MSSubClass.45 + MSSubClass.60 + 
         MSSubClass.70 + MSSubClass.75 + MSSubClass.85 + MSSubClass.90 + 
         MSSubClass.120 + MSZoning.C..all. + MSZoning.FV + MSZoning.RH + 
         MSZoning.RL + LotShape.IR1 + LotConfig.Corner + LotConfig.CulDSac + 
         LotConfig.FR2 + Neighborhood.Blueste + Neighborhood.BrDale + 
         Neighborhood.BrkSide + Neighborhood.ClearCr + Neighborhood.Crawfor + 
         Neighborhood.Edwards + Neighborhood.IDOTRR + Neighborhood.NoRidge + 
         Neighborhood.NPkVill + Neighborhood.NridgHt + Neighborhood.StoneBr + 
         Condition1.Artery + Condition1.Feedr + Condition1.RRAe + 
         Condition1.RRAn + HouseStyle.2.5Fin + HouseStyle.2Story + 
         Exterior1st.BrkFace + Exterior1st.CemntBd + Exterior1st.Wd.Sdng + 
         Exterior2nd.CmentBd + Exterior2nd.MetalSd + Exterior2nd.VinylSd + 
         Exterior2nd.Wd.Sdng + MasVnrType.BrkCmn + MasVnrType.BrkFace + 
         MasVnrType.None + ExterQual.Ex + ExterQual.Gd + ExterCond.Ex + 
         Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + 
         Foundation.Slab + Foundation.Stone + BsmtQual.Ex + BsmtQual.Fa + 
         BsmtCond.Fa + BsmtCond.Po + BsmtExposure.Av + BsmtExposure.Gd + 
         BsmtExposure.Mn + BsmtFinType1.GLQ + HeatingQC.Ex + CentralAir.N + 
         Electrical.Mix + KitchenQual.Ex + GarageCond.Fa + GarageCond.Po + 
         PavedDrive.N + MoSold.1 + MoSold.2 + MoSold.3 + MoSold.4 + 
         MoSold.8 + MoSold.9 + MoSold.10 + MoSold.11 + SaleType.New + 
         SaleType.Oth + SaleCondition.Abnorml + SaleCondition.Alloca + 
         SaleCondition.Family + SaleCondition.Normal + LotArea + OverallQual + 
         OverallCond + YearBuilt + YearRemodAdd + TotalBsmtSF + X1stFlrSF + 
         X2ndFlrSF + GrLivArea + BsmtFullBath + FullBath + HalfBath + 
         TotRmsAbvGrd + Fireplaces + GarageYrBlt + GarageCars + WoodDeckSF + 
         TotLivArea + AvgRmSz + Exterior2nd.Plywood, data=HPtrn)

TrnRMSE5<-sqrt(mean((HPtrn$SalePrice-m5$fitted.values)^2))
m5tstpred<-predict(m5, HPtst)
TstRMSE5<-sqrt(mean((HPtst$SalePrice-m5tstpred)^2))

# Train RMSE
TrnRMSE5

# Test RMSE
TstRMSE5


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

# Model 2 gbm

set.seed(2345)

m6<-train(SalePrice ~ ., data=HPtrn, method="gbm", trControl=fitControl,
          verbose=F, tuneGrid=gbmGrid, metric="RMSE")

m6trnpred<-predict(m6, HPtrn)
TrnRMSE6<-sqrt(mean((HPtrn$SalePrice-m6trnpred)^2))
m6tstpred<-predict(m6, HPtst)
TstRMSE6<-sqrt(mean((HPtst$SalePrice-m6tstpred)^2))


# Train RMSE
TrnRMSE6

# Test RMSE
TstRMSE6


### Model 3 Random Forest

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

# Model 3 Random Forest

set.seed(3456)

m7<- train(SalePrice ~ ., data=HPtrn, method="rf", trControl=fitControl,
           verbose=F, metric="RMSE")

m7trnpred<-predict(m7, HPtrn)
TrnRMSE7<-sqrt(mean((HPtrn$SalePrice-m7trnpred)^2))
m7tstpred<-predict(m7, HPtst)
TstRMSE7<-sqrt(mean((HPtst$SalePrice-m7tstpred)^2))

# Train RMSE
TrnRMSE7

# Test RMSE
TstRMSE7

### Model 8 MARS


## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

# Model 8 Multivariate Adaptive Regression Spline (MARS)

set.seed(4567)

m8<- train(SalePrice ~ ., data=HPtrn, method="earth", 
           trControl=fitControl, metric="RMSE", tuneLength=5)


m8trnpred<-predict(m8, HPtrn)
TrnRMSE8<-sqrt(mean((HPtrn$SalePrice-m8trnpred)^2))
m8tstpred<-predict(m8, HPtst)
TstRMSE8<-sqrt(mean((HPtst$SalePrice-m8tstpred)^2))

# Train RMSE
TrnRMSE8

# Test RMSE
TstRMSE8

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
pr5<-predict(m5, testing)
pr6<-predict(m6, testing)
pr7<-predict(m7, testing)
pr8<-predict(m8, testing)

s5<-as.data.frame(cbind(Id, Saleprice=exp(pr5)))
s6<-as.data.frame(cbind(Id, SalePrice=exp(pr6)))
s7<-as.data.frame(cbind(Id, SalePrice=exp(pr7)))
s8<-as.data.frame(cbind(Id, SalePrice=exp(pr8)))
names(s8)<-c("Id", "SalePrice")

write.csv(s5, "C:/Kaggle/HousePrices/s5.csv", row.names = F)
write.csv(s6, "C:/Kaggle/HousePrices/s6.csv", row.names = F)
write.csv(s7, "C:/Kaggle/HousePrices/s7.csv", row.names = F)
write.csv(s8, "C:/Kaggle/HousePrices/s8.csv", row.names = F)



