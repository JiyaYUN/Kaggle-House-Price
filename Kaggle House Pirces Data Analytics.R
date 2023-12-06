remove(list=ls())
setwd("D:/Simon.UR/Fall A/MGC461 Professional Communication")
train <- read.csv("train.csv")
train <- train[, -which(names(train) %in% c("Alley", "FireplaceQu", "PoolQC", "Fence", "MiscFeature","LotFrontage"))]
train <- train[complete.cases(train$MasVnrType), ]
train <- train[complete.cases(train$BsmtCond), ]
train <- train[complete.cases(train$BsmtExposure), ]
train <- train[complete.cases(train$BsmtFinType2), ]
train <- train[complete.cases(train$Electrical), ]
train <- train[complete.cases(train$GarageType), ]
summary(train)
str(train)

numeric_columns <- train[sapply(train, is.numeric)]
cor_matrix <- cor(numeric_columns)
correlation <- cor(numeric_columns)[,"SalePrice"]
correlation <- correlation[order(correlation, decreasing = TRUE)]

set.seed(20)
isTraining = runif(nrow(train))<.8
trainingData = subset(train,isTraining)
validationData = subset(train,!isTraining)

mean((predict(lm(SalePrice ~ ., data = trainingData), newdata = validationData) - validationData$SalePrice)^2)
summary(lm(SalePrice ~ MSZoning + LotArea + LandSlope + Neighborhood + OverallQual + OverallCond +
                YearBuilt + RoofMatl + MasVnrType + ExterQual + BsmtQual + BsmtExposure + BsmtFinType1 +
                X1stFlrSF + KitchenQual + GarageQual, data = trainingData))

str(trainingData)
