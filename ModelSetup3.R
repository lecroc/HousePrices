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

PreObj<-preProcess(HPtrn[,2:250], method = c("zv", "center", "scale"))
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

m9<-lm(SalePrice ~ MSSubClass.20 + MSSubClass.50 + MSSubClass.60 + MSSubClass.70 + 
         MSSubClass.75 + MSSubClass.85 + MSSubClass.90 + MSSubClass.160 + 
         MSZoning.C..all. + MSZoning.FV + MSZoning.RH + MSZoning.RL + 
         LotShape.IR1 + LotConfig.CulDSac + Neighborhood.CollgCr + 
         Neighborhood.Crawfor + Neighborhood.Edwards + Neighborhood.Gilbert + 
         Neighborhood.MeadowV + Neighborhood.Mitchel + Neighborhood.NAmes + 
         Neighborhood.NridgHt + Neighborhood.NWAmes + Neighborhood.OldTown + 
         Neighborhood.Sawyer + Neighborhood.StoneBr + Neighborhood.SWISU + 
         Neighborhood.Timber + Condition1.Artery + Condition1.Feedr + 
         Condition1.RRAe + BldgType.Twnhs + HouseStyle.1.5Fin + HouseStyle.1Story + 
         HouseStyle.2.5Fin + HouseStyle.2Story + HouseStyle.SFoyer + 
         RoofStyle.Gable + RoofStyle.Hip + Exterior1st.BrkFace + Exterior1st.CemntBd + 
         Exterior1st.MetalSd + Exterior1st.Wd.Sdng + Exterior2nd.Brk.Cmn + 
         Exterior2nd.CmentBd + Exterior2nd.VinylSd + Exterior2nd.Wd.Sdng + 
         MasVnrType.BrkCmn + MasVnrType.BrkFace + MasVnrType.None + 
         ExterCond.Fa + Foundation.BrkTil + Foundation.CBlock + Foundation.PConc + 
         Foundation.Slab + Foundation.Stone + BsmtQual.Ex + BsmtQual.Fa + 
         BsmtCond.Fa + BsmtExposure.Gd + BsmtFinType1.GLQ + BsmtFinType1.LwQ + 
         HeatingQC.Ex + CentralAir.N + Electrical.FuseA + KitchenQual.Ex + 
         GarageType.2Types + GarageCond.Fa + GarageCond.Po + PavedDrive.N + 
         MoSold.5 + MoSold.6 + MoSold.7 + SaleType.ConLD + SaleType.CWD + 
         SaleType.Oth + SaleCondition.Abnorml + SaleCondition.Family + 
         SaleCondition.Normal + LotFrontage + LotArea + OverallQual + 
         OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + 
         TotalBsmtSF + X1stFlrSF + X2ndFlrSF + BsmtFullBath + FullBath + 
         HalfBath + TotRmsAbvGrd + Fireplaces + GarageYrBlt + GarageCars + 
         GarageArea + WoodDeckSF + YrSold + AvgRmSz + BsmtExposure.No + 
         HouseStyle.1.5Unf, data=HPtrn)

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


## Model 13 xgBoost

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)


trdf<- data.table(HPtrn, keep.rownames=F)
tedf<-data.table(HPtst, keep.rownames = F)

xgb.grid <- expand.grid(nrounds = 500,
                        max_depth = seq(6,10),
                        eta = c(0.01,0.3, 1),
                        gamma = c(0.0, 0.2, 1),
                        colsample_bytree = c(0.5,0.8, 1)
)


set.seed(4321)

m13 <-train(SalePrice ~.,
                 data=trdf,
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=fitControl
                 
)

# oad("C:/Kaggle/HousePrices/m13.RData")

m13trnpred<-predict(m13, HPtrn)
TrnRMSE13<-sqrt(mean((HPtrn$SalePrice-m13trnpred)^2))
m13tstpred<-predict(m13, HPtst)
TstRMSE13<-sqrt(mean((HPtst$SalePrice-m13tstpred)^2))



# Train RMSE
TrnRMSE13

# Test RMSE
TstRMSE13



Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
pr9<-predict(m9, testing)
pr10<-predict(m10, testing)
pr11<-predict(m11, testing)
pr12<-predict(m12, testing)
pr13<-predict(m13, testing)

s9<-as.data.frame(cbind(Id, Saleprice=exp(pr9)))
s10<-as.data.frame(cbind(Id, SalePrice=exp(pr10)))
s11<-as.data.frame(cbind(Id, SalePrice=exp(pr11)))
s12<-as.data.frame(cbind(Id, SalePrice=exp(pr12)))
names(s12)<-c("Id", "SalePrice")
s13<-as.data.frame(cbind(Id, SalePrice=exp(pr13)))

write.csv(s9, "C:/Kaggle/HousePrices/s9.csv", row.names = F)
write.csv(s10, "C:/Kaggle/HousePrices/s10.csv", row.names = F)
write.csv(s11, "C:/Kaggle/HousePrices/s11.csv", row.names = F)
write.csv(s12, "C:/Kaggle/HousePrices/s12.csv", row.names = F)
write.csv(s13, "C:/Kaggle/HousePrices/s13.csv", row.names = F)


