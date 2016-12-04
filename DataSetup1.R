##### Code for Post 3 on Home Sale Price Prediction


### Load Data

library(forcats)

hdata<-read.csv("C:/Kaggle/House Prices/train.csv")
hdata2<-read.csv("C:/Kaggle/House Prices/test.csv")
hdata2$SalePrice<-0

# Combine train and test for data cleansing

hdata<-rbind(hdata,hdata2)


################ Data Cleanup and conversions #################

## MSSubClass -> convert to factor and break into binaries:
hdata$MSSubClass<-as.factor(hdata$MSSubClass)
hdata$MSSub20 = as.numeric(hdata$MSSubClass==20)
hdata$MSSub60 = as.numeric(hdata$MSSubClass==60)
hdata$MSSub50 = as.numeric(hdata$MSSubClass==50)
hdata$MSSub120 = as.numeric(hdata$MSSubClass==120)
hdata$MSSub30 = as.numeric(hdata$MSSubClass==30)
hdata$MSSub160 = as.numeric(hdata$MSSubClass==160)
hdata$MSSub70 = as.numeric(hdata$MSSubClass==70)
hdata$MSSub80 = as.numeric(hdata$MSSubClass==80)
hdata$MSSub90 = as.numeric(hdata$MSSubClass==90)
hdata<-subset(hdata, select=-MSSubClass)

## MSZoning -> break into binaries:
hdata$MSZRL<-as.numeric(hdata$MSZoning=="RL")
hdata$MSZRM<-as.numeric(hdata$MSZoning=="RM")
hdata$MSZFV<-as.numeric(hdata$MSZoning=="FV")
hdata<-subset(hdata, select=-MSZoning)

## Street to binary
hdata$STR<-as.numeric(hdata$Street=="Pave")
hdata<-subset(hdata, select=-Street)

## Alley to binary
hdata$ALLGRVL<-as.numeric(hdata$Alley=="Grvl")
hdata$ALLPAVE<-as.numeric(hdata$Alley=="Pave")
hdata<-subset(hdata, select=-Alley)

## LotShape to binary
hdata$LOTSHREG<-as.numeric(hdata$LotShape=="Reg")
hdata$LOTSHIR1<-as.numeric(hdata$LotShape=="IR1")
hdata$LOTSHIR2<-as.numeric(hdata$LotShape=="IR2")
hdata<-subset(hdata, select=-LotShape)

## LandContour to binary
hdata$LNDCNTRLVL<-as.numeric(hdata$LandContour=="Lvl")
hdata$LNDCNTRBNK<-as.numeric(hdata$LandContour=="Bnk")
hdata$LNDCNTRHLS<-as.numeric(hdata$LandContour=="HLS")
hdata<-subset(hdata, select=-LandContour)

## Utilities to binary
hdata$UTILALL<-as.numeric(hdata$Utilities=="AllPub")
hdata<-subset(hdata, select=-Utilities)

## LotConfig to binary
hdata$LOTCFGIN<-as.numeric(hdata$LotConfig=="Inside")
hdata$LOTCFGCRN<-as.numeric(hdata$LotConfig=="Corner")
hdata$LOTCFGCUL<-as.numeric(hdata$LotConfig=="CulDSac")
hdata$LOTCFGFR<-as.numeric(hdata$LotConfig=="FR2")
hdata<-subset(hdata, select=-LotConfig)

## LandSlope to binary
hdata$SLOPEGTL<-as.numeric(hdata$LandSlope=="Gtl")
hdata$SLOPEMOD<-as.numeric(hdata$LandSlope=="Mod")
hdata<-subset(hdata, select=-LandSlope)

## Condition1 to binary
hdata$COND1NORM<-as.numeric(hdata$Condition1=="Norm")
hdata$COND1FEED<-as.numeric(hdata$Condition1=="Feedr")
hdata$COND1ART<-as.numeric(hdata$Condition1=="Artery")
hdata<-subset(hdata, select=-Condition1)

## Condition2 to binary
hdata$COND2NORM<-as.numeric(hdata$Condition2=="Norm")
hdata$COND2FEED<-as.numeric(hdata$Condition2=="Feedr")
hdata$COND2ART<-as.numeric(hdata$Condition2=="Artery")
hdata<-subset(hdata, select=-Condition2)

## BldgType to binary
hdata$BTP1FAM<-as.numeric(hdata$BldgType=="1Fam")
hdata$BTPTHE<-as.numeric(hdata$BldgType=="TwnhsE")
hdata$BTPDUP<-as.numeric(hdata$BldgType=="Duplex")
hdata$BTPTH<-as.numeric(hdata$BldgType=="Twnhs")
hdata<-subset(hdata, select=-BldgType)

## HouseStyle to binary
hdata$BTP1FAM<-as.numeric(hdata$HouseStyle=="1Story")
hdata$BTPTHE<-as.numeric(hdata$HouseStyle=="2Story")
hdata$BTPDUP<-as.numeric(hdata$HouseStyle=="1.5Fin")
hdata$BTPTH<-as.numeric(hdata$HouseStyle=="SLvl")
hdata$BTPTH<-as.numeric(hdata$HouseStyle=="SFoyer")
hdata<-subset(hdata, select=-HouseStyle)

## RoofStyle to binary
hdata$ROOFGBL<-as.numeric(hdata$RoofStyle=="Gable")
hdata$ROOFHIP<-as.numeric(hdata$RoofStyle=="Hip")
hdata<-subset(hdata, select=-RoofStyle)

## RoofMatl to binary
hdata$ROOFM<-as.numeric(hdata$RoofMatl=="CompShg")
hdata<-subset(hdata, select=-RoofMatl)

## Exterior1st to binary
hdata$X1VYNL = as.numeric(hdata$Exterior1st=="VinylSd")
hdata$X1HDBRD = as.numeric(hdata$Exterior1st=="HdBoard")
hdata$X1MTLSD = as.numeric(hdata$Exterior1st=="MetalSd")
hdata$X1WDSD = as.numeric(hdata$Exterior1st=="Wd Sdng")
hdata$X1PLY = as.numeric(hdata$Exterior1st=="Plywood")
hdata$X1CEMBD = as.numeric(hdata$Exterior1st=="CemntBd")
hdata$X1BRKF = as.numeric(hdata$Exterior1st=="BrkFace")
hdata$X1WDSHNG = as.numeric(hdata$Exterior1st=="WdShing")
hdata$X1STCO = as.numeric(hdata$Exterior1st=="Stucco")
hdata$X1ASBSHNG = as.numeric(hdata$Exterior1st=="AsbShng")
hdata<-subset(hdata, select=-Exterior1st)

## Exterior2nd to binary
hdata$X2VYNL = as.numeric(hdata$Exterior2nd=="VinylSd")
hdata$X2HDBRD = as.numeric(hdata$Exterior2nd=="HdBoard")
hdata$X2MTLSD = as.numeric(hdata$Exterior2nd=="MetalSd")
hdata$X2WDSD = as.numeric(hdata$Exterior2nd=="Wd Sdng")
hdata$X2PLY = as.numeric(hdata$Exterior2nd=="Plywood")
hdata$X2CEMBD = as.numeric(hdata$Exterior2nd=="CemntBd")
hdata$X2BRKF = as.numeric(hdata$Exterior2nd=="BrkFace")
hdata$X2WDSHNG = as.numeric(hdata$Exterior2nd=="WdShing")
hdata$X2STCO = as.numeric(hdata$Exterior2nd=="Stucco")
hdata$X2ASBSHNG = as.numeric(hdata$Exterior2nd=="AsbShng")
hdata<-subset(hdata, select=-Exterior2nd)

## MasVnrType to binary
hdata$MASVNONE<-as.numeric(hdata$MasVnrType=="None")
hdata$MASVBRKF<-as.numeric(hdata$MasVnrType=="BrkFace")
hdata$MASVST<-as.numeric(hdata$MasVnrType=="Stone")
hdata<-subset(hdata, select=-MasVnrType)

## ExterQual to binary
hdata$XQAVG<-as.numeric(hdata$ExterQual=="TA")
hdata$XQGD<-as.numeric(hdata$ExterQual=="Gd")
hdata$XQEX<-as.numeric(hdata$ExterQual=="Ex")
hdata<-subset(hdata, select=-ExterQual)

## ExterCond to binary
hdata$XCOAVG<-as.numeric(hdata$ExterCond=="TA")
hdata$XCOGD<-as.numeric(hdata$ExterCond=="Gd")
hdata$XCOFA<-as.numeric(hdata$ExterCond=="Fa")
hdata<-subset(hdata, select=-ExterCond)

## Foundation to binary
hdata$FOUNDPC<-as.numeric(hdata$Foundation=="Pconc")
hdata$FOUNDCB<-as.numeric(hdata$Foundation=="CBlock")
hdata$FOUNDBRTL<-as.numeric(hdata$Foundation=="BrkTil")
hdata$FOUNDSLB<-as.numeric(hdata$Foundation=="Slab")
hdata<-subset(hdata, select=-Foundation)

## BsmtQual to binary
hdata$BQUALAVG<-as.numeric(hdata$BsmtQual=="TA")
hdata$BQUALGD<-as.numeric(hdata$BsmtQual=="Gd")
hdata$BQUALEX<-as.numeric(hdata$BsmtQual=="Ex")
hdata$BQUALFA<-as.numeric(hdata$BsmtQual=="Fa")
hdata<-subset(hdata, select=-BsmtQual)

## BsmtCond to binary
hdata$BCONAVG<-as.numeric(hdata$BsmtCond=="TA")
hdata$BCONGD<-as.numeric(hdata$BsmtCond=="Gd")
hdata$BCONFA<-as.numeric(hdata$BsmtCond=="Ex")
hdata<-subset(hdata, select=-BsmtCond)

## BsmtExposure to binary
hdata$BMEXNO<-as.numeric(hdata$BsmtExposure=="No")
hdata$BMEXAVG<-as.numeric(hdata$BsmtExposure=="Av")
hdata$BMEXGD<-as.numeric(hdata$BsmtExposure=="Gd")
hdata$BMEXMN<-as.numeric(hdata$BsmtExposure=="Mn")
hdata<-subset(hdata, select=-BsmtExposure)

## BsmtFinType1 to binary
hdata$BFT1UNF = as.numeric(hdata$BsmtFinType1=="Unf")
hdata$BFT1GLQ = as.numeric(hdata$BsmtFinType1=="GLQ")
hdata$BFT1ALQ = as.numeric(hdata$BsmtFinType1=="ALQ")
hdata$BFT1BLQ = as.numeric(hdata$BsmtFinType1=="BLQ")
hdata$BFT1REC = as.numeric(hdata$BsmtFinType1=="Rec")
hdata$BFT1LOW = as.numeric(hdata$BsmtFinType1=="LwQ")
hdata<-subset(hdata, select=-BsmtFinType1)

## BsmtFinType2 to binary
hdata$BFT2UNF = as.numeric(hdata$BsmtFinType2=="Unf")
hdata$BFT2BLQ = as.numeric(hdata$BsmtFinType2=="BLQ")
hdata$BFT2REC = as.numeric(hdata$BsmtFinType2=="Rec")
hdata$BFT2LOW = as.numeric(hdata$BsmtFinType2=="LwQ")
hdata<-subset(hdata, select=-BsmtFinType2)

## Heating to binary
hdata$HEATGASA<-as.numeric(hdata$Heating=="GasA")
hdata<-subset(hdata, select=-Heating)

## HeatingQC to binary
hdata$HQCEX = as.numeric(hdata$HeatingQC=="Ex")
hdata$HQTA = as.numeric(hdata$HeatingQC=="TA")
hdata$HQGD = as.numeric(hdata$HeatingQC=="Gd")
hdata$HQFA = as.numeric(hdata$HeatingQC=="Fa")
hdata<-subset(hdata, select=-HeatingQC)

## CentralAir to binary
hdata$CENTAIRY<-as.numeric(hdata$CentralAir=="Y")
hdata<-subset(hdata, select=-CentralAir)

## Electrical to binary
hdata$ELECSB<-as.numeric(hdata$Electrical=="SBrkr")
hdata$ELECFA<-as.numeric(hdata$Electrical=="FuseA")
hdata$ELECFF<-as.numeric(hdata$Electrical=="FuseF")
hdata<-subset(hdata, select=-Electrical)

## KitchenQual to binary
hdata$KITCHQTA = as.numeric(hdata$KitchenQual=="TA")
hdata$KITCHQGD = as.numeric(hdata$KitchenQual=="Gd")
hdata$KITCHQEX = as.numeric(hdata$KitchenQual=="Ex")
hdata$KITCHQFA = as.numeric(hdata$KitchenQual=="Fa")
hdata<-subset(hdata, select=-KitchenQual)

## Functional to binary
hdata$FUNCTYP<-as.numeric(hdata$Functional=="Typ")
hdata$FUNCMN2<-as.numeric(hdata$Functional=="Min2")
hdata$FUNCMN1<-as.numeric(hdata$Functional=="Min1")
hdata<-subset(hdata, select=-Functional)

## FireplaceQu to binary
hdata$FPQNONE = as.numeric(hdata$FireplaceQu=="None")
hdata$FPQGD = as.numeric(hdata$FireplaceQu=="Gd")
hdata$FPQTA = as.numeric(hdata$FireplaceQu=="TA")
hdata$FPQFA = as.numeric(hdata$FireplaceQu=="Fa")
hdata$FPQEX = as.numeric(hdata$FireplaceQu=="Ex")
hdata<-subset(hdata, select=-FireplaceQu)

## GarageType to binary
hdata$GTYPATTCH<-as.numeric(hdata$GarageType=="Attchd")
hdata$GTYDET<-as.numeric(hdata$GarageType=="Detchd")
hdata$GTYBI<-as.numeric(hdata$GarageType=="BuiltIn")
hdata<-subset(hdata, select=-GarageType)

## GarageFinish to binary
hdata$GFINUNF<-as.numeric(hdata$GarageFinish=="Unf")
hdata$GRFINRFN<-as.numeric(hdata$GarageFinish=="RFn")
hdata$GRFINFIN<-as.numeric(hdata$GarageFinish=="Fin")
hdata<-subset(hdata, select=-GarageFinish)

## GarageQual to binary
hdata$GQUALTA<-as.numeric(hdata$GarageQual=="TA")
hdata$GQUALFA<-as.numeric(hdata$GarageQual=="Fa")
hdata<-subset(hdata, select=-GarageQual)

## GarageCond to binary
hdata$GARCOTA<-as.numeric(hdata$GarageCond=="TA")
hdata$GARCOFA<-as.numeric(hdata$GarageCond=="Fa")
hdata<-subset(hdata, select=-GarageCond)

## PavedDrive to binary
hdata$PAVDRV<-as.numeric(hdata$PavedDrive)
hdata<-subset(hdata, select=-PavedDrive)

## Fence to binary
fct_explicit_na(hdata$Fence, "None")
hdata$Fence<-as.numeric(hdata$Fence!="None")
hdata$Fence[is.na(hdata$Fence)]<-0

## MiscFeature to binary
fct_explicit_na(hdata$MiscFeature, "None")
hdata$MiscFeature<-as.numeric(hdata$MiscFeature!="None")
hdata$MiscFeature[is.na(hdata$MiscFeature)]<-0

## MoSold -> convert to factor and break into binaries:
hdata$MoSold<-as.factor(hdata$MoSold)
hdata$MSJUN = as.numeric(hdata$MoSold==6)
hdata$MSJUL = as.numeric(hdata$MoSold==7)
hdata$MSMAY = as.numeric(hdata$MoSold==5)
hdata$MSAPR = as.numeric(hdata$MoSold==4)
hdata$MSAUG = as.numeric(hdata$MoSold==8)
hdata$MSMAR = as.numeric(hdata$MoSold==3)
hdata$MSOCT = as.numeric(hdata$MoSold==10)
hdata$MSNOV = as.numeric(hdata$MoSold==11)
hdata$MSSEPT = as.numeric(hdata$MoSold==9)
hdata$MSDEC = as.numeric(hdata$MoSold==12)
hdata$MSJAN = as.numeric(hdata$MoSold==1)
hdata<-subset(hdata, select=-MoSold)

## SaleType to binary
hdata$SLTYPWD<-as.numeric(hdata$SaleType=="WD")
hdata$SLTYPNEW<-as.numeric(hdata$SaleType=="New")
hdata$SLTYPCOD<-as.numeric(hdata$SaleType=="COD")
hdata<-subset(hdata, select=-SaleType)

## SaleCondition to binary
hdata$SLCONDNORM<-as.numeric(hdata$SaleCondition=="Normal")
hdata$SLCONDPART<-as.numeric(hdata$SaleCondition=="Partial")
hdata$SLCONDAB<-as.numeric(hdata$SaleCondition=="Abnorml")
hdata<-subset(hdata, select=-SaleCondition)

## OverallQual -> convert to factor and break into binaries:
hdata$OverallQual<-as.factor(hdata$OverallQual)
hdata$OAQUAL2 = as.numeric(hdata$OverallQual==2)
hdata$OAQUAL3 = as.numeric(hdata$OverallQual==3)
hdata$OAQUAL4 = as.numeric(hdata$OverallQual==4)
hdata$OAQUAL5 = as.numeric(hdata$OverallQual==5)
hdata$OAQUAL6 = as.numeric(hdata$OverallQual==6)
hdata$OAQUAL7 = as.numeric(hdata$OverallQual==7)
hdata$OAQUAL8 = as.numeric(hdata$OverallQual==8)
hdata$OAQUAL9 = as.numeric(hdata$OverallQual==9)
hdata$OAQUAL10 = as.numeric(hdata$OverallQual==10)
hdata<-subset(hdata, select=-OverallQual)

## OverallCond -> convert to factor and break into binaries:
hdata$OverallCond<-as.factor(hdata$OverallCond)
hdata$OACOND2 = as.numeric(hdata$OverallCond==2)
hdata$OACOND3 = as.numeric(hdata$OverallCond==3)
hdata$OACOND4 = as.numeric(hdata$OverallCond==4)
hdata$OACOND5 = as.numeric(hdata$OverallCond==5)
hdata$OACOND6 = as.numeric(hdata$OverallCond==6)
hdata$OACOND7 = as.numeric(hdata$OverallCond==7)
hdata$OACOND8 = as.numeric(hdata$OverallCond==8)
hdata$OACOND9 = as.numeric(hdata$OverallCond==9)
hdata$OACOND10 = as.numeric(hdata$OverallCond==10)
hdata<-subset(hdata, select=-OverallCond)

## YearBuilt to binary
hdata$YB30 = as.numeric(hdata$YearBuilt>1929 & hdata$YearBuilt<1940)
hdata$YB40 = as.numeric(hdata$YearBuilt>1939 & hdata$YearBuilt<1950)
hdata$YB50 = as.numeric(hdata$YearBuilt>1949 & hdata$YearBuilt<1960)
hdata$YB60 = as.numeric(hdata$YearBuilt>1959 & hdata$YearBuilt<1970)
hdata$YB70 = as.numeric(hdata$YearBuilt>1969 & hdata$YearBuilt<1980)
hdata$YB80 = as.numeric(hdata$YearBuilt>1979 & hdata$YearBuilt<1990)
hdata$YB90 = as.numeric(hdata$YearBuilt>1989 & hdata$YearBuilt<2000)
hdata$YB00 = as.numeric(hdata$YearBuilt>1999 & hdata$YearBuilt<2010)
hdata$YB10 = as.numeric(hdata$YearBuilt>2010)
hdata<-subset(hdata, select=-YearBuilt)

## YearRemodAdd to binary
hdata$YRM60 = as.numeric(hdata$YearRemodAdd>1959 & hdata$YearRemodAdd<1970)
hdata$YRM70 = as.numeric(hdata$YearRemodAdd>1969 & hdata$YearRemodAdd<1980)
hdata$YRM80 = as.numeric(hdata$YearRemodAdd>1979 & hdata$YearRemodAdd<1990)
hdata$YRM90 = as.numeric(hdata$YearRemodAdd>1989 & hdata$YearRemodAdd<2000)
hdata$YRM00 = as.numeric(hdata$YearRemodAdd>1999 & hdata$YearRemodAdd<2010)
hdata$YRM10 = as.numeric(hdata$YearRemodAdd>2010)
hdata<-subset(hdata, select=-YearRemodAdd)

## BsmtFullBath to binary
hdata$BMFB1<-as.numeric(hdata$BsmtFullBath==1)
hdata$BMFB2 <-as.numeric(hdata$BsmtFullBath>1)
hdata<-subset(hdata, select=-BsmtFullBath)

## BsmtHalfBath to binary
hdata$BMHB1 = as.numeric(hdata$BsmtHalfBath==1)
hdata$BMHB2 = as.numeric(hdata$BsmtHalfBath>1)
hdata<-subset(hdata, select=-BsmtHalfBath)

## FullBath to binary
hdata$FB1 = as.numeric(hdata$FullBath==1)
hdata$FB2 = as.numeric(hdata$FullBath==2)
hdata$FB3 = as.numeric(hdata$FullBath>2)
hdata<-subset(hdata, select=-FullBath)

## HalfBath to binary
hdata$HB1 = as.numeric(hdata$HalfBath==1)
hdata$HB2 = as.numeric(hdata$HalfBath>1)
hdata<-subset(hdata, select=-HalfBath)

## BedroomAbvGr to binary
hdata$BRAG1 = as.numeric(hdata$BedroomAbvGr==1)
hdata$BRAG2 = as.numeric(hdata$BedroomAbvGr==2)
hdata$BRAG3 = as.numeric(hdata$BedroomAbvGr==3)
hdata$BRAG4 = as.numeric(hdata$BedroomAbvGr>3)
hdata<-subset(hdata, select=-BedroomAbvGr)

## KitchenAbvGr to binary
hdata$KAG1 = as.numeric(hdata$KitchenAbvGr==1)
hdata$KAG2 = as.numeric(hdata$KitchenAbvGr>1)
hdata<-subset(hdata, select=-KitchenAbvGr)

## Fireplaces to binary
hdata$FRPL1 = as.numeric(hdata$Fireplaces==1)
hdata$FRPL2 = as.numeric(hdata$Fireplaces>1)
hdata<-subset(hdata, select=-Fireplaces)

## GarageYrBlt to binary
hdata$GARYBOLD = as.numeric(hdata$GarageYrBlt>0 & hdata$GarageYrBlt<1980)
hdata$GARYBMED = as.numeric(hdata$GarageYrBlt>1979 & hdata$GarageYrBlt<2000)
hdata$GARYBNEW = as.numeric(hdata$GarageYrBlt>1999)
hdata<-subset(hdata, select=-GarageYrBlt)

## YrSold to binary
hdata$YRSOLD2007<-as.numeric(hdata$YrSold=="2007")
hdata$YRSOLD2008<-as.numeric(hdata$YrSold=="2008")
hdata$YRSOLD2009<-as.numeric(hdata$YrSold=="2009")
hdata$YRSOLD2010<-as.numeric(hdata$YrSold=="2010")
hdata<-subset(hdata, select=-YrSold)

## drop PoolQC
hdata<-subset(hdata, select=-PoolQC)

## Replace NA with 0

hdata[is.na(hdata)]<-0

## confirm we have 2919 complete cases

sum(complete.cases(hdata))

## log transform select variales
cols<-c("LotArea", "LotFrontage", "MasVnrArea", "BsmtFinSF1", "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "GrLivArea", "GarageArea", "WoodDeckSF", "SalePrice")
hdata[cols]<-log1p(hdata[cols])

## Seperate train and test
hdatatrain<-subset(hdata, SalePrice>0)
hdatatest<-subset(hdata, SalePrice==0)

# Remove SalePrice from test
hdatatest<-subset(hdatatest, select=-SalePrice)

rmv<-read.csv("C:/Kaggle/House Prices/rmv.csv")
rmv<-as.numeric(rmv[,2])
hdatatrain<-hdatatrain[-rmv,] # new data frame for model without outliers

# Remove Id variable and stash test set Ids
Id<-as.data.frame(hdatatest$Id)
names(Id)<-c("Id")
hdatatrain<-subset(hdatatrain, select=-Id)
hdatatest<-subset(hdatatest, select=-Id)

write.csv(hdatatrain, "C:/Kaggle/House Prices/train1.csv", row.names = F)
write.csv(hdatatest, "C:/Kaggle/House Prices/test1.csv", row.names = F)
write.csv(Id, "C:/Kaggle/House Prices/testId.csv", row.names = F)