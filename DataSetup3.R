### Load Data

library(forcats)
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

# Read in data

training<-read.csv("C:/Kaggle/House Prices/train.csv", na.strings = "NA")
testing<-read.csv("C:/Kaggle/House Prices/test.csv", na.strings = "NA")

# Stash testing Id
Id<-testing$Id

# Add SalePrice to testing

testing$SalePrice<-0

# Combine training and testing for PreProssessing

Combo<-as.data.frame(rbind(training, testing))

# Drop columns more than 50% NA

Combo<-Combo[, -which(colMeans(is.na(Combo))>.6)]

# Impute columns with NA

Combo<-knnImputation(Combo, k=3)

# Confirm no NA

sum(complete.cases(Combo))

# Remove columns with near zero variance
nzv_cols <- nearZeroVar(Combo)
if(length(nzv_cols) > 0) Combo <- Combo[, -nzv_cols]

# Switch some numberics to factors
Combo$MoSold<-as.factor(Combo$MoSold)
Combo$MSSubClass<-as.factor(Combo$MSSubClass)

# Separate numeric and factor variables

nums<-sapply(Combo, is.numeric)
numerics<-Combo[, nums]
factors<-Combo[, !nums]

# log transform select numerics

cols<-c("LotArea", "LotFrontage", "MasVnrArea", "BsmtFinSF1", "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "GrLivArea", "GarageArea", "WoodDeckSF", "SalePrice")
numerics[cols]<-log1p(numerics[, cols])

# Remove Id
numerics<-numerics[,2:27]

# dummify factors
dmy <- dummyVars(" ~ .", data = factors)
dummies <- data.frame(predict(dmy, newdata = factors))

# Break Out training and testing
Combo<-as.data.frame(cbind(dummies, numerics))
training<-subset(Combo, Combo$SalePrice>0)
testing<-subset(Combo, Combo$SalePrice==0)
testing$SalePrice<-NULL

