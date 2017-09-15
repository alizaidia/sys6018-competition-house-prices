#Non-Parametric Form solution to the Kaggle Housing Prices Competition
#Pragati Shah, Ali Zaidi, James Xie

#Read in the data 
train1 = read.table('train.csv', sep = ',', header = T, stringsAsFactors = T)
train_SalePrice = train1[,81]
train1 = train1[,-c(1,81)]

#Set NA values to 0
train1[is.na(train1)] = '0'

test = read.table('test.csv', sep = ',', header = T, stringsAsFactors = T)
Id<-test[,1]
test = test[,-1]

#Check if same dimensions for train and test data
dim(train1)
dim(test)

com = rbind(train1, test)
library(dplyr)
glimpse(com)

#Replace NA values with centrality statistic
install.packages("DMwR")
library(DMwR)
com = centralImputation(com)

#Check if any NAs left
sum(is.na(com))

num_col = c('MSSubClass', 'LotFrontage', 'LotArea', 'YearBuilt', 'YearRemodAdd',
            'OverallQual', 'OverallCond', 
            'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 
            'X1stFlrSF', 'X2ndFlrSF','1stFlrSF', '2ndFlrSF', 'LowQualFinSF', 
            'GrLivArea', 'TotRmsAbvGrd', 'GarageYrBlt','GarageCars', 'GarageArea', 
            'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', '3SsnPorch', 'ScreenPorch', 
            'PoolArea', 'MiscVal')

#Change columns to respective types - numeric or factor
for(i in names(com)){
  if (i %in% num_col){
    com[,i] = as.numeric(com[,i])
  }
  else{com[,i] = as.factor(com[,i])}
}
sum(is.na(com))

#Use random forest on data - this model was chosen because it uses collection of decision trees as to not overfit the data. 
#Random forest was also used due to the number of variables in this data set, SVM is only good for two class problems. GBM
#wasn't used due to the amount of noise in the data - too much noise causes more overfit with GBM. 
install.packages("randomForest")
library(randomForest)
train_final <-cbind(com[1:1460,],train_SalePrice)
test_final = com[1461:nrow(com),]

dim(train_final)
dim(test_final)

rf = randomForest(train_SalePrice~., data = train_final, mtry=10, ntree=35)

SalePrice = predict(rf, newdata = test_final)
pred<-cbind(Id,SalePrice)
write.csv(pred, file = "pred_parametric.csv",row.names=FALSE)

#Ensemble Model - average values from two models

SalePrice<-0.5*(test_new[,"SalePrice"]+pred[,"SalePrice"])
Id<-test_new$Id
Ensemble<-cbind(Id,SalePrice)
write.csv(Ensemble, file = "pred_ensemble.csv",row.names=FALSE)
