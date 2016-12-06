### Model House Prices

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

training<-read.csv("C:/Kaggle/HousePrices/train1.csv")
testing<-read.csv("C:/Kaggle/HousePrices/test1.csv")

## Create train and validation data sets from training data
set.seed (1234)
inTrain<-createDataPartition(y=training$SalePrice, p=.85, list=F)
HPtrn<-training[inTrain,]
HPtst<-training[-inTrain,]

m1<-lm(SalePrice ~ LotFrontage + LotArea + Neighborhood : GrLivArea + BsmtFinSF1 +
         BsmtUnfSF + X1stFlrSF + X2ndFlrSF + GarageCars + 
         WoodDeckSF + EnclosedPorch + ScreenPorch + MiscFeature + 
         MSSub20 + MSSub60 + MSSub160 + MSSub90 + MSZRL + MSZRM + 
         MSZFV + STR + LOTSHREG + LNDCNTRBNK + UTILALL + LOTCFGIN + 
         LOTCFGCRN + LOTCFGCUL + SLOPEGTL + SLOPEMOD + COND1NORM + 
         COND2NORM + COND2FEED + X1VYNL + X1HDBRD + X1MTLSD + X1WDSD + 
         X1PLY + X1CEMBD + X1BRKF + X1WDSHNG + X1STCO + X1ASBSHNG + 
         X2PLY + X2BRKF + XQAVG + XQGD + XQEX + XCOAVG + XCOGD + XCOFA + 
         FOUNDBRTL + BQUALAVG + BQUALGD + BQUALEX + BQUALFA + BMEXGD + 
         BFT1GLQ + BFT1ALQ + BFT1BLQ + BFT1REC + BFT2UNF + BFT2BLQ + 
         BFT2REC + BFT2LOW + HQCEX + CENTAIRY + KITCHQEX + FUNCTYP + 
         FUNCMN2 + FUNCMN1 + GTYPATTCH + GTYDET + GTYBI + GQUALTA + 
         GQUALFA + GARCOTA + MSJUN + MSJUL + MSMAY + SLTYPWD + SLTYPCOD + 
         SLCONDNORM + SLCONDAB + OAQUAL3 + OAQUAL4 + OAQUAL5 + OAQUAL6 + 
         OAQUAL7 + OAQUAL8 + OAQUAL9 + OAQUAL10 + OACOND2 + OACOND3 + 
         OACOND4 + OACOND5 + OACOND7 + OACOND8 + OACOND9 + YB30 + 
         YB50 + YB60 + YB70 + YB80 + YB90 + YB00 + YRM90 + BMFB1 + 
         BMFB2 + BMHB1 + FB2 + FB3 + HB1 + KAG2 + FRPL1 + FRPL2 + 
         YRSOLD2009 + BTPTH + GFINUNF + BRAG4, data=HPtrn)

TrnRMSE1<-sqrt(mean((HPtrn$SalePrice-m1$fitted.values)^2))
m1tstpred<-predict(m1, HPtst)
TstRMSE1<-sqrt(mean((HPtst$SalePrice-m1tstpred)^2))

# Train RMSE
TrnRMSE1

# Test RMSE
TstRMSE1


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

m2<-train(SalePrice~LotFrontage + LotArea + Neighborhood : GrLivArea + BsmtFinSF1 +
            BsmtUnfSF + X1stFlrSF + X2ndFlrSF + GarageCars + 
            WoodDeckSF + EnclosedPorch + ScreenPorch + MiscFeature + 
            MSSub20 + MSSub60 + MSSub160 + MSSub90 + MSZRL + MSZRM + 
            MSZFV + STR + LOTSHREG + LNDCNTRBNK + UTILALL + LOTCFGIN + 
            LOTCFGCRN + LOTCFGCUL + SLOPEGTL + SLOPEMOD + COND1NORM + 
            COND2NORM + COND2FEED + X1VYNL + X1HDBRD + X1MTLSD + X1WDSD + 
            X1PLY + X1CEMBD + X1BRKF + X1WDSHNG + X1STCO + X1ASBSHNG + 
            X2PLY + X2BRKF + XQAVG + XQGD + XQEX + XCOAVG + XCOGD + XCOFA + 
            FOUNDBRTL + BQUALAVG + BQUALGD + BQUALEX + BQUALFA + BMEXGD + 
            BFT1GLQ + BFT1ALQ + BFT1BLQ + BFT1REC + BFT2UNF + BFT2BLQ + 
            BFT2REC + BFT2LOW + HQCEX + CENTAIRY + KITCHQEX + FUNCTYP + 
            FUNCMN2 + FUNCMN1 + GTYPATTCH + GTYDET + GTYBI + GQUALTA + 
            GQUALFA + GARCOTA + MSJUN + MSJUL + MSMAY + SLTYPWD + SLTYPCOD + 
            SLCONDNORM + SLCONDAB + OAQUAL3 + OAQUAL4 + OAQUAL5 + OAQUAL6 + 
            OAQUAL7 + OAQUAL8 + OAQUAL9 + OAQUAL10 + OACOND2 + OACOND3 + 
            OACOND4 + OACOND5 + OACOND7 + OACOND8 + OACOND9 + YB30 + 
            YB50 + YB60 + YB70 + YB80 + YB90 + YB00 + YRM90 + BMFB1 + 
            BMFB2 + BMHB1 + FB2 + FB3 + HB1 + KAG2 + FRPL1 + FRPL2 + 
            YRSOLD2009 + BTPTH + GFINUNF + BRAG4, data=HPtrn, method="gbm", trControl=fitControl,
          verbose=F, tuneGrid=gbmGrid, metric="RMSE")

# load("C:/Kaggle/HousePrices/m2.RData")

m2trnpred<-predict(m2, HPtrn)
TrnRMSE2<-sqrt(mean((HPtrn$SalePrice-m2trnpred)^2))
m2tstpred<-predict(m2, HPtst)
TstRMSE2<-sqrt(mean((HPtst$SalePrice-m2tstpred)^2))


# Train RMSE
TrnRMSE2

# Test RMSE
TstRMSE2


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

m3<- train(SalePrice~LotFrontage + LotArea + Neighborhood : GrLivArea + BsmtFinSF1 +
             BsmtUnfSF + X1stFlrSF + X2ndFlrSF + GarageCars + 
             WoodDeckSF + EnclosedPorch + ScreenPorch + MiscFeature + 
             MSSub20 + MSSub60 + MSSub160 + MSSub90 + MSZRL + MSZRM + 
             MSZFV + STR + LOTSHREG + LNDCNTRBNK + UTILALL + LOTCFGIN + 
             LOTCFGCRN + LOTCFGCUL + SLOPEGTL + SLOPEMOD + COND1NORM + 
             COND2NORM + COND2FEED + X1VYNL + X1HDBRD + X1MTLSD + X1WDSD + 
             X1PLY + X1CEMBD + X1BRKF + X1WDSHNG + X1STCO + X1ASBSHNG + 
             X2PLY + X2BRKF + XQAVG + XQGD + XQEX + XCOAVG + XCOGD + XCOFA + 
             FOUNDBRTL + BQUALAVG + BQUALGD + BQUALEX + BQUALFA + BMEXGD + 
             BFT1GLQ + BFT1ALQ + BFT1BLQ + BFT1REC + BFT2UNF + BFT2BLQ + 
             BFT2REC + BFT2LOW + HQCEX + CENTAIRY + KITCHQEX + FUNCTYP + 
             FUNCMN2 + FUNCMN1 + GTYPATTCH + GTYDET + GTYBI + GQUALTA + 
             GQUALFA + GARCOTA + MSJUN + MSJUL + MSMAY + SLTYPWD + SLTYPCOD + 
             SLCONDNORM + SLCONDAB + OAQUAL3 + OAQUAL4 + OAQUAL5 + OAQUAL6 + 
             OAQUAL7 + OAQUAL8 + OAQUAL9 + OAQUAL10 + OACOND2 + OACOND3 + 
             OACOND4 + OACOND5 + OACOND7 + OACOND8 + OACOND9 + YB30 + 
             YB50 + YB60 + YB70 + YB80 + YB90 + YB00 + YRM90 + BMFB1 + 
             BMFB2 + BMHB1 + FB2 + FB3 + HB1 + KAG2 + FRPL1 + FRPL2 + 
             YRSOLD2009 + BTPTH + GFINUNF + BRAG4, data=HPtrn, method="rf", trControl=fitControl,
           verbose=F, metric="RMSE")


# load("C:/Kaggle/HousePrices/m3.RData")

m3trnpred<-predict(m3, HPtrn)
TrnRMSE3<-sqrt(mean((HPtrn$SalePrice-m3trnpred)^2))
m3tstpred<-predict(m3, HPtst)
TstRMSE3<-sqrt(mean((HPtst$SalePrice-m3tstpred)^2))

# Train RMSE
TrnRMSE3

# Test RMSE
TstRMSE3

### Model 4 MARS


## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

# Model 4 Multivariate Adaptive Regression Spline (MARS)

set.seed(4567)

m4<- train(SalePrice~LotFrontage + LotArea + Neighborhood : GrLivArea + BsmtFinSF1 +
             BsmtUnfSF + X1stFlrSF + X2ndFlrSF + GarageCars + 
             WoodDeckSF + EnclosedPorch + ScreenPorch + MiscFeature + 
             MSSub20 + MSSub60 + MSSub160 + MSSub90 + MSZRL + MSZRM + 
             MSZFV + STR + LOTSHREG + LNDCNTRBNK + UTILALL + LOTCFGIN + 
             LOTCFGCRN + LOTCFGCUL + SLOPEGTL + SLOPEMOD + COND1NORM + 
             COND2NORM + COND2FEED + X1VYNL + X1HDBRD + X1MTLSD + X1WDSD + 
             X1PLY + X1CEMBD + X1BRKF + X1WDSHNG + X1STCO + X1ASBSHNG + 
             X2PLY + X2BRKF + XQAVG + XQGD + XQEX + XCOAVG + XCOGD + XCOFA + 
             FOUNDBRTL + BQUALAVG + BQUALGD + BQUALEX + BQUALFA + BMEXGD + 
             BFT1GLQ + BFT1ALQ + BFT1BLQ + BFT1REC + BFT2UNF + BFT2BLQ + 
             BFT2REC + BFT2LOW + HQCEX + CENTAIRY + KITCHQEX + FUNCTYP + 
             FUNCMN2 + FUNCMN1 + GTYPATTCH + GTYDET + GTYBI + GQUALTA + 
             GQUALFA + GARCOTA + MSJUN + MSJUL + MSMAY + SLTYPWD + SLTYPCOD + 
             SLCONDNORM + SLCONDAB + OAQUAL3 + OAQUAL4 + OAQUAL5 + OAQUAL6 + 
             OAQUAL7 + OAQUAL8 + OAQUAL9 + OAQUAL10 + OACOND2 + OACOND3 + 
             OACOND4 + OACOND5 + OACOND7 + OACOND8 + OACOND9 + YB30 + 
             YB50 + YB60 + YB70 + YB80 + YB90 + YB00 + YRM90 + BMFB1 + 
             BMFB2 + BMHB1 + FB2 + FB3 + HB1 + KAG2 + FRPL1 + FRPL2 + 
             YRSOLD2009 + BTPTH + GFINUNF + BRAG4, data=HPtrn, method="earth", 
           trControl=fitControl, metric="RMSE", tuneLength=5)


# load("C:/Kaggle/HousePrices/m4.RData")

m4trnpred<-predict(m4, HPtrn)
TrnRMSE4<-sqrt(mean((HPtrn$SalePrice-m4trnpred)^2))
m4tstpred<-predict(m4, HPtst)
TstRMSE4<-sqrt(mean((HPtst$SalePrice-m4tstpred)^2))

# Train RMSE
TrnRMSE4

# Test RMSE
TstRMSE4

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")
pr2<-predict(m2, testing)
pr3<-predict(m3, testing)
pr4<-predict(m4, testing)

s2<-as.data.frame(cbind(Id, SalePrice=exp(pr2)))
s3<-as.data.frame(cbind(Id, SalePrice=exp(pr3)))
s4<-as.data.frame(cbind(Id, SalePrice=exp(pr4)))
names(s4)<-c("Id", "SalePrice")

write.csv(s2, "C:/Kaggle/HousePrices/s2.csv", row.names = F)
write.csv(s3, "C:/Kaggle/HousePrices/s3.csv", row.names = F)
write.csv(s4, "C:/Kaggle/HousePrices/s4.csv", row.names = F)



