#Parametric Form solution to the Kaggle Housing Prices Competition
#Pragati Shah, Ali Zaidi, James Xie

#library
library(readr)  
library(dplyr)  

#Reading and viewing data set
house = read_csv("train.csv")
head(house)
glimpse(house)

#Find how many NAs in each column
apply(house, 2, function(col) sum(is.na(col)))

#What are unique values in each column
l <- sapply(house, function(x) is.character(x))
m <- house[, l]
n <- sapply(m, function(x)  (unique(x)))

#Change Invalid Names - can't begin with letter
house$GarageType[house$GarageType=="2Types"]<-"TwoTypes"

house$HouseStyle[house$HouseStyle=="2Story"]<-"TwoStory"
house$HouseStyle[house$HouseStyle=="1Story"]<-"OneStory"
house$HouseStyle[house$HouseStyle=="1.5Fin"]<-"Fin1.5"
house$HouseStyle[house$HouseStyle=="1.5Unf"]<-"Unf1.5"
house$HouseStyle[house$HouseStyle=="2.5Unf"]<-"Unf2.5"
house$HouseStyle[house$HouseStyle=="2.5Fin"]<-"Fin2.5"

house$BldgType[house$BldgType=="1Fam"]<-"Fam1"
house$BldgType[house$BldgType=="2fmCon"]<-"fmCon2"

#Fixes Names of Invalid Columns
names(house) <- make.names(names(house))

# features <- setdiff(colnames(house), c("Id", "SalePrice"))
# for (f in features) {
#   # if (any(is.na(house[[f]]))) 
#   #   if (is.character(house[[f]])){ 
#   #     house[[f]][is.na(house[[f]])] <- "Others"
#   #   }else{
#   #     house[[f]][is.na(house[[f]])] <- -0  
#   #   }
#    if (is.character(house[[f]])){ 
#     house[[f]]<-as.factor(house[[f]])
#   }
# }

glimpse(house)
dim(house)

#Exploratory Data Analysis
#Plotting histogram of SalePrice - data is heavily skewed right 
hist(house$SalePrice)

#House price is normalized usiong logarithmic transformation - allows models such as linear regression to perform better
house$SalePrice_log=log(house$SalePrice+1)
hist(house$SalePrice_log)

#Analyzing the relationship between different variables and sales price
boxplot(SalePrice~Neighborhood,data=house)
boxplot(SalePrice_log~Neighborhood,data=house)

boxplot(SalePrice~BldgType,data=house)
boxplot(SalePrice_log~BldgType,data=house)

install.packages("corrplot")
library(corrplot)
l <- sapply(house, function(x) !is.character(x))
M <- cor(house[,l])
corrplot(M) #Use correlation plot to determine which variables show the best relationships

#Data Preprocessing
#Checking to determine whether all levels in factor attributes in test dataset are a subset of the ones present in train 
#data set.
#Reading and viewing data set
test = read_csv("test.csv")
head(test)
glimpse(test)

test$GarageType[test$GarageType=="2Types"]<-"TwoTypes" 

test$HouseStyle[test$HouseStyle=="2Story"]<-"TwoStory"
test$HouseStyle[test$HouseStyle=="1Story"]<-"OneStory"
test$HouseStyle[test$HouseStyle=="1.5Fin"]<-"Fin1.5"
test$HouseStyle[test$HouseStyle=="1.5Unf"]<-"Unf1.5"
test$HouseStyle[test$HouseStyle=="2.5Unf"]<-"Unf2.5"
test$HouseStyle[test$HouseStyle=="2.5Fin"]<-"Fin2.5"

test$BldgType[test$BldgType=="1Fam"]<-"Fam1"
test$BldgType[test$BldgType=="2fmCon"]<-"fmCon2"
names(test) <- make.names(names(test))

l <- sapply(test, function(x) is.character(x))
m <- test[, l]
n_test <- sapply(m, function(x)  (unique(x)))
glimpse(n_test)
class(n_test)

test_train_diff<-n_test
for (f in 1:length(n)){

  test_train_diff[[f]]<- n_test[[f]][which(!n_test[[f]]%in%n[[f]])]
}
test_train_diff

#Caret package looks at near zero predictor values - looking at variables that mostly only have one value
install.packages("caret")
library(caret)
nzv <- nearZeroVar(house, saveMetrics= TRUE)
dim(house)
req_cols <- nzv[nzv$nzv==FALSE,]
house_filtered<-house[,rownames(req_cols)]
dim(house_filtered)
glimpse(house_filtered)

apply(house_filtered, 2, function(col) sum(is.na(col)))

#Fill in NULLs in case of factor attributes - some factors are NA but have another meaning
house_filtered$Alley[is.na(house_filtered$Alley)]<-"No Access"
house_filtered$FireplaceQu[is.na(house_filtered$FireplaceQu)]<-"No Fireplace"
house_filtered$PoolQC[is.na(house_filtered$PoolQC)]<-"No Pool"
house_filtered$Fence[is.na(house_filtered$Fence)]<-"No Fence"
house_filtered$GarageType[is.na(house_filtered$GarageType)]<-"No Garage"
house_filtered$GarageYrBlt[is.na(house_filtered$GarageYrBlt)]<-0
house_filtered$GarageFinish[is.na(house_filtered$GarageFinish)]<-"No Garage"
house_filtered$BsmtQual[is.na(house_filtered$BsmtQual)]<-"No Basement"
house_filtered$BsmtFinType1[is.na(house_filtered$BsmtFinType1)]<-"No Basement"
house_filtered$BsmtExposure[is.na(house_filtered$BsmtExposure)]<-0

#Label NAs as "others" or 0
features <- setdiff(colnames(house_filtered), c("Id", "SalePrice"))
for (f in features) {
  if (any(is.na(house_filtered[[f]])))
    if (is.character(house_filtered[[f]])){
      house_filtered[[f]][is.na(house_filtered[[f]])] <- "Others"
    }else{
      house_filtered[[f]][is.na(house_filtered[[f]])] <- -0
    }
  if (is.character(house_filtered[[f]])){
    house_filtered[[f]]<-as.factor(house_filtered[[f]])
  }
}
glimpse(house_filtered)


#Use e1071 to apply the skewness and understand how skewed eaech variable is 
install.packages("e1071")
library(e1071) 
house_filtered$MSSubClass<-as.factor(house_filtered$MSSubClass)

l <- sapply(house_filtered, is.numeric)
m <- house_filtered[, l]
skewness_attr <- sapply(m, function(x)  (skewness(x)))
skewed<-m[,skewness_attr>0.75]
skewed$SalePrice<-NULL
glimpse(skewed)
names(skewed)

a<-setdiff(colnames(house_filtered),names(skewed))
m_factor<-house_filtered[,a]

log_numeric <- sapply(skewed, function(x) log(x+1))
house_new<-cbind(m_factor,log_numeric)

glimpse(house_new)
dim(house_new)



#Use the MASS library to use stepAIC for the best model
lm1<-lm(SalePrice_log ~.-SalePrice-Id, data=house_new)
summary(lm1)
install.packages("MASS")
library(MASS)
model1.stepAIC <- stepAIC(lm1, direction=c("both"))

lm2<-lm(SalePrice_log ~MSSubClass + MSZoning + LotFrontage + LotShape + 
          LotConfig + Neighborhood + Condition1 + OverallQual + OverallCond + 
          YearBuilt + YearRemodAdd + Exterior1st + MasVnrType + ExterCond + 
          Foundation + BsmtQual + BsmtExposure + HeatingQC + CentralAir + 
          BsmtFullBath + FullBath + HalfBath + KitchenQual + Fireplaces + 
          GarageCars + PoolQC + YrSold + SaleType + SaleCondition + 
          LotArea + BsmtFinSF1 + BsmtUnfSF + TotalBsmtSF + GrLivArea + 
          WoodDeckSF, data=house_new)
summary(lm2)


#################Prediction####################################
#Apply same pre-processing from train data to the test data
test$SalePrice<-0
test$SalePrice_log<-0
test_filtered<-test[,rownames(req_cols)]
dim(test_filtered)
glimpse(test_filtered)

test_filtered$Alley[is.na(test_filtered$Alley)]<-"No Access"
test_filtered$FireplaceQu[is.na(test_filtered$FireplaceQu)]<-"No Fireplace"
test_filtered$PoolQC[is.na(test_filtered$PoolQC)]<-"No Pool"
test_filtered$Fence[is.na(test_filtered$Fence)]<-"No Fence"
test_filtered$GarageType[is.na(test_filtered$GarageType)]<-"No Garage"
test_filtered$GarageYrBlt[is.na(test_filtered$GarageYrBlt)]<-0
test_filtered$GarageFinish[is.na(test_filtered$GarageFinish)]<-"No Garage"
test_filtered$BsmtQual[is.na(test_filtered$BsmtQual)]<-"No Basement"
test_filtered$BsmtFinType1[is.na(test_filtered$BsmtFinType1)]<-"No Basement"
test_filtered$BsmtExposure[is.na(test_filtered$BsmtExposure)]<-0



features <- setdiff(colnames(test_filtered), c("Id", "SalePrice"))
for (f in features) {
  if (any(is.na(test_filtered[[f]])))
    if (is.character(test_filtered[[f]])){
      test_filtered[[f]][is.na(test_filtered[[f]])] <- "Others"
    }else{
      test_filtered[[f]][is.na(test_filtered[[f]])] <- -0
    }
  if (is.character(test_filtered[[f]])){
    test_filtered[[f]]<-as.factor(test_filtered[[f]])
  }
}
glimpse(test_filtered)

#Add log transformation to the test data
test_log_columns=test_filtered[,names(skewed)]
log_test <- sapply(test_log_columns, function(x) log(x+1))

test_factor<-test_filtered[,a]

test_new<-cbind(test_factor,log_test)
test_new$MSSubClass<-as.factor(test_new$MSSubClass)
dim(test_new)

test_new$MSZoning[test_new$MSZoning=="Others"]<-"RL"
test_new$MSSubClass[test_new$MSSubClass==150]<-20
test_new$Exterior1st[test_new$Exterior1st=="Others"]<-"VinylSd"
test_new$Exterior2nd[test_new$Exterior2nd=="Others"]<-"VinylSd"

test_new$KitchenQual[test_new$KitchenQual=="Others"]<-"TA"
test_new$SaleType[test_new$SaleType=="Others"]<-"WD"

test_new$MSZoning=droplevels(test_new$MSZoning)
test_new$KitchenQual=droplevels(test_new$KitchenQual)
test_new$SaleType=droplevels(test_new$SaleType)

#Now making predictions (parametric) - using logistic regression model due to skewness of data
test_new$SalePrice_log <- predict(lm2, newdata=test_new)
test_new$SalePrice<-exp(test_new$SalePrice_log)-1
# Write p1.predict to CSV 
write.csv(test_new[,c("Id","SalePrice")], file = "predictions1.csv",row.names=FALSE)