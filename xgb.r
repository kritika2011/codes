
install.packages("randomForest")
install.packages("gbm")
install.packages("rpart")
install.packages("dplyr")
install.packages("caret")
install.packages("pROC")
install.packages("e1071")
install.packages("gplots")
install.packages("mltools")
install.packages("ROCR")
install.packages("rpart.plot")
install.packages("tidyverse")
install.packages("MLmetrics")
install.packages("fastAdaboost")
install.packages("mlbench")
install.packages("mltools")
#install.packages("ModelMetrics")


#loading packages

library(randomForest)
library(gbm)
library(rpart)
library(dplyr)
library(caret)
library(pROC)
library(e1071)
library(gplots)
library(mltools)
library(ROCR)
library(tidyverse)
library(MLmetrics)
library(fastAdaboost)
library(mlbench)
library(mltools)


setwd("/global/home/mma_kdixit/Finance")
RawData <- read.csv("Bankruptcy_data_Final.csv", as.is = TRUE)
df <- RawData

set.seed(823)
n <- nrow(df)
shuffled <- df[sample(n), ]
trn <-shuffled[1:round(0.7*n), ]
val <-shuffled[(round(0.7*n)+1):(round(0.9*n)), ]
test <-shuffled[(round(0.9*n)+1):n, ]


table(df$BK)
table(trn$BK)
table(val$BK)
table(test$BK)

summary(trn)
dim(trn)
str(trn)

trn$Tobin.s.Q <- replace_na(trn$Tobin.s.Q, 1.54)
trn$EPS <- replace_na(trn$Tobin.s.Q, 0.3)
trn$Liquidity <- replace_na(trn$Liquidity, 0.190)
trn$Profitability <- replace_na(trn$Profitability, 0.07)
trn$Productivity <- replace_na(trn$Productivity, 0.06)
trn$Leverage.Ratio <- replace_na(trn$Leverage.Ratio, 0.280)
trn$Asset.Turnover <- replace_na(trn$Asset.Turnover, 0.820)
trn$Operational.Margin <- replace_na(trn$Operational.Margin, 0.060)
trn$Return.on.Equity <- replace_na(trn$Return.on.Equity, 0.03)
trn$Market.Book.Ratio <- replace_na(trn$Market.Book.Ratio, 58)
trn$Assets.Growth <- replace_na(trn$Assets.Growth, 0.052)
trn$Sales.Growth <- replace_na(trn$Sales.Growth, 0.06)
trn$Employee.Growth <- replace_na(trn$Employee.Growth, 0.017)

trn$Tobin.s.Q <- ifelse(trn$Tobin.s.Q > 10.8, 10.8, trn$Tobin.s.Q)
trn$EPS <- ifelse(trn$EPS > 10.8, 10.8, trn$EPS)
trn$Profitability <- ifelse(trn$Profitability > 1.48991, 1.48991, trn$Profitability)
trn$Profitability <- ifelse(trn$Profitability < -4.46, -4.46, trn$Profitability)
trn$Productivity <- ifelse(trn$Productivity > 1.18964, 1.18964, trn$Productivity)
trn$Leverage.Ratio <- ifelse(trn$Leverage.Ratio > 2.71, 2.71, trn$Leverage.Ratio)
trn$Asset.Turnover <- ifelse(trn$Asset.Turnover > 2.67, 2.67, trn$Asset.Turnover)
trn$Operational.Margin <- ifelse(trn$Operational.Margin > 0.96, 0.96, trn$Operational.Margin)
trn$Return.on.Equity <- ifelse(trn$Return.on.Equity > 7.97928, 7.97928, trn$Return.on.Equity)
trn$Market.Book.Ratio <- ifelse(trn$Market.Book.Ratio > 31032.46, 31032.46, trn$Market.Book.Ratio)
trn$Market.Book.Ratio <- ifelse(trn$Market.Book.Ratio < 6.74, 6.74, trn$Market.Book.Ratio)
trn$Assets.Growth <- ifelse(trn$Assets.Growth > 4.643, 4.643, trn$Assets.Growth)
trn$Sales.Growth <- ifelse(trn$Sales.Growth > 0.906, 0.906, trn$Sales.Growth)
trn$Employee.Growth <- ifelse(trn$Employee.Growth > 0.625, 0.625, trn$Employee.Growth)

val$Tobin.s.Q <- replace_na(val$Tobin.s.Q, 1.54)
val$EPS <- replace_na(val$Tobin.s.Q, 0.3)
val$Liquidity <- replace_na(val$Liquidity, 0.190)
val$Profitability <- replace_na(val$Profitability, 0.07)
val$Productivity <- replace_na(val$Productivity, 0.06)
val$Leverage.Ratio <- replace_na(val$Leverage.Ratio, 0.280)
val$Asset.Turnover <- replace_na(val$Asset.Turnover, 0.820)
val$Operational.Margin <- replace_na(val$Operational.Margin, 0.060)
val$Return.on.Equity <- replace_na(val$Return.on.Equity, 0.03)
val$Market.Book.Ratio <- replace_na(val$Market.Book.Ratio, 58)
val$Assets.Growth <- replace_na(val$Assets.Growth, 0.052)
val$Sales.Growth <- replace_na(val$Sales.Growth, 0.06)
val$Employee.Growth <- replace_na(val$Employee.Growth, 0.017)


trn$Tobin.s.Q <- ifelse(trn$Tobin.s.Q > 10.8, 10.8, trn$Tobin.s.Q)
trn$EPS <- ifelse(trn$EPS > 10.8, 10.8, trn$EPS)
trn$Profitability <- ifelse(trn$Profitability > 1.48991, 1.48991, trn$Profitability)
trn$Profitability <- ifelse(trn$Profitability < -4.46, -4.46, trn$Profitability)
trn$Productivity <- ifelse(trn$Productivity > 1.18964, 1.18964, trn$Productivity)
trn$Leverage.Ratio <- ifelse(trn$Leverage.Ratio > 2.71, 2.71, trn$Leverage.Ratio)
trn$Asset.Turnover <- ifelse(trn$Asset.Turnover > 2.67, 2.67, trn$Asset.Turnover)
trn$Operational.Margin <- ifelse(trn$Operational.Margin > 0.96, 0.96, trn$Operational.Margin)
trn$Return.on.Equity <- ifelse(trn$Return.on.Equity > 7.97928, 7.97928, trn$Return.on.Equity)
trn$Market.Book.Ratio <- ifelse(trn$Market.Book.Ratio > 31032.46, 31032.46, trn$Market.Book.Ratio)
trn$Market.Book.Ratio <- ifelse(trn$Market.Book.Ratio < 6.74, 6.74, trn$Market.Book.Ratio)
trn$Assets.Growth <- ifelse(trn$Assets.Growth > 4.643, 4.643, trn$Assets.Growth)
trn$Sales.Growth <- ifelse(trn$Sales.Growth > 0.906, 0.906, trn$Sales.Growth)
trn$Employee.Growth <- ifelse(trn$Employee.Growth > 0.625, 0.625, trn$Employee.Growth)


trn1 <- trn[, -1]
val1 <- val[, -1]


trn2 <- trn1[, -1]
val2 <- val1[, -1]





trn2$BK <-as.factor(trn2$BK)




y<-sapply(trn2,class)
y
levels(trn2$BK) <- c("non_bankcrupt", "bankcrupt")


trainX <-trn2[,-13]
install.packages("xgboost")
library(xgboost)
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,search="random",
                     allowParallel = TRUE)

xgb.grid <- expand.grid(.nrounds = c(200,250,300,350,400,450,500,550,600,650,700,800), 
                        .eta = c(0.05,0.01,0.02,0.05,0.1))

xgb.tune <-train(x=trainX,y=trn2$BK,
                 method="xgbTree",
                 metric="ROC",
                 trControl=ctrl,tuneLength=15)


print(xgb.tune)
plot(xgb.tune)  		# Plot the performance of the training models

