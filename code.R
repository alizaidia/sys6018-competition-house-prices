library(dplyr)
library(tidyr)
library(readr)

train <- read.csv("train.csv")
glimpse(train)

test <- read.csv("test.csv")
glimpse(test)

#check for missing values
na.vals <- colSums(is.na(train))
na.vals

model <- lm(SalePrice ~ OverallQual + OverallCond, data = train)
summary(model)

new_data_test <- data.frame(test$OverallQual, test$OverallCond)
pred <- predict(model, test)
