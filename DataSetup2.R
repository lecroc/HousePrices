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

# Read in data

training<-read.csv("C:/Kaggle/House Prices/train.csv", na.strings = "NA")
testing<-read.csv("C:/Kaggle/House Prices/test.csv", na.strings = "NA")

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

# Create some new variables
Combo$TotLivArea<-Combo$GrLivArea+Combo$BsmtFinSF1
Combo$AvgRmSz<-Combo$GrLivArea/Combo$TotRmsAbvGrd
Combo$RemodAge<-Combo$YrSold-Combo$YearRemodAdd
Combo$RemodAge<-pmax(Combo$RemodAge, 0)
  
  
# Remove columns with near zero variance
nzv_cols <- nearZeroVar(Combo)
if(length(nzv_cols) > 0) Combo <- Combo[, -nzv_cols]

# Convert select variables to factor

Combo$MSSubClass<-as.factor(Combo$MSSubClass)
Combo$MoSold<-as.factor(Combo$MoSold)


# Separate numeric and factor variables

nums<-sapply(Combo, is.numeric)
numerics<-Combo[, nums]
factors<-Combo[, !nums]

# Remove Id
numerics<-numerics[,2:30]

# log transform numerics
numerics<-log1p(numerics)

# dummify factors
dmy <- dummyVars(" ~ .", data = factors)
dummies <- data.frame(predict(dmy, newdata = factors))

# Reassemble Combo
Combo<-as.data.frame(cbind(dummies, numerics))

# Re-order columns so SalePrice is first
Combo<-Combo[, c(247, 1:246, 248:250)]

# Break Out training and testing
training<-subset(Combo, Combo$SalePrice>0)
testing<-subset(Combo, Combo$SalePrice==0)
testing$SalePrice<-NULL

rmv<-read.csv("C:/Kaggle/HousePrices/rmv.csv")
rmv<-as.numeric(rmv[,2])
training<-training[-rmv,] # new data frame for model without outliers

write.csv(training, file="C:/Kaggle/HousePrices/train2.csv", row.names = F)
write.csv(testing, file="C:/Kaggle/HousePrices/test2.csv", row.names = F)

