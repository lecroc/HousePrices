### Model House Prices 4

# Load libraries
library(MASS)
library(caret)
library(caretEnsemble)
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

seed<-1234

# Get data

training<-read.csv("C:/Kaggle/HousePrices/train3.csv")
testing<-read.csv("C:/Kaggle/HousePrices/test3.csv")

PreObj<-preProcess(training[,2:250], method = c("zv", "center", "scale"))
trntrans<-predict(PreObj, training[, 2:250])
training<-as.data.frame(cbind(SalePrice=log(training$SalePrice), trntrans))
testing<-predict(PreObj, testing)

# Stacked model ensemble

# Set up training control
fitControl<-trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = "final")

# Set up model list
modellist<-c("gbm", "bagEarthGCV", "bayesglm", "cubist")

# run models

## initialize for parallel processing

library(doSNOW)
getDoParWorkers()
registerDoSNOW(makeCluster(7, type="SOCK"))
getDoParWorkers()
getDoParName()
library(foreach)

set.seed(seed)

models<-caretList(SalePrice~., data=training, metric="RMSE", trControl = fitControl, methodList = modellist)

results<-resamples(models)

summary(results)
dotplot(results)

modelCor(results)
splom(results)

# brnnn stack model

set.seed(seed)
stackbrnn <- caretStack(models, method="brnn", metric="RMSE", trControl=fitControl)
print(stackbrnn)

stbpred<-predict(stackbrnn, testing)

Id<-read.csv("C:/Kaggle/HousePrices/testId.csv")

stksub<-as.data.frame(cbind(Id, SalePrice=exp(stbpred)))

stksub$SalePrice<-round(stksub$SalePrice, -2)


# check names / str of dfs

write.csv(stksub, "C:/Kaggle/HousePrices/stksubbrnn.csv", row.names = F)
