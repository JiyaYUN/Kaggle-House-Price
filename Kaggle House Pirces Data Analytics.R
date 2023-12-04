remove(list=ls())
setwd("D:/Simon.UR/Fall A/MGC461 Professional Communication")
test <- read.csv("test.csv")
train <- read.csv("train.csv")

str(train)
library(dplyr)
train <- train %>%
     mutate(
          MSZoning = as.factor(MSZoning),
          Street = as.factor(Street),
          Alley = as.factor(Alley),
          LotShape = as.factor(LotShape),
          LandContour = as.factor(LandContour),
          Utilities = as.factor(Utilities),
          LotConfig = as.factor(LotConfig),
          LandSlope = as.factor(LandSlope),
          Neighborhood = as.factor(Neighborhood),
          Condition1 = as.factor(Condition1),
          Condition2 = as.factor(Condition2),
          BldgType = as.factor(BldgType),
          HouseStyle = as.factor(HouseStyle),
          RoofStyle = as.factor(RoofStyle),
          RoofMatl = as.factor(RoofMatl),
          Exterior1st = as.factor(Exterior1st),
          Exterior2nd = as.factor(Exterior2nd),
          MasVnrType = as.factor(MasVnrType),
          ExterQual = as.factor(ExterQual),
          ExterCond = as.factor(ExterCond),
          Foundation = as.factor(Foundation),
          BsmtQual = as.factor(BsmtQual),
          BsmtCond = as.factor(BsmtCond),
          BsmtExposure = as.factor(BsmtExposure),
          BsmtFinType1 = as.factor(BsmtFinType1),
          BsmtFinType2 = as.factor(BsmtFinType2),
          Heating = as.factor(Heating),
          HeatingQC = as.factor(HeatingQC),
          CentralAir = as.factor(CentralAir),
          Electrical = as.factor(Electrical),
          KitchenQual = as.factor(KitchenQual),
          Functional = as.factor(Functional),
          FireplaceQu = as.factor(FireplaceQu),
          GarageType = as.factor(GarageType),
          GarageFinish = as.factor(GarageFinish),
          GarageQual = as.factor(GarageQual),
          GarageCond = as.factor(GarageCond),
          PavedDrive = as.factor(PavedDrive),
          PoolQC = as.factor(PoolQC),
          Fence = as.factor(Fence),
          MiscFeature = as.factor(MiscFeature),
          SaleType = as.factor(SaleType),
          SaleCondition = as.factor(SaleCondition)
     )


train <- train[, -which(names(train) %in% c("Alley", "FireplaceQu", "PoolQC", "Fence", "MiscFeature","LotFrontage"))]
train <- train[complete.cases(train$MasVnrType), ]
train <- train[complete.cases(train$BsmtCond), ]
train <- train[complete.cases(train$BsmtExposure), ]
train <- train[complete.cases(train$BsmtFinType2), ]
train <- train[complete.cases(train$Electrical), ]
train <- train[complete.cases(train$GarageType), ]
summary(train)
str(train)

library(ggplot2)
library(reshape2)

numeric_columns <- train[sapply(train, is.numeric)]
cor_matrix <- cor(numeric_columns)

ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
     geom_tile() +
     scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
     labs(title = "Correlation Heatmap")

correlation <- cor(numeric_columns)[,"SalePrice"]
correlation <- correlation[order(correlation, decreasing = TRUE)]

ggplot(data = data.frame(Feature = names(correlation), Correlation = correlation),
       aes(x = Feature, y = Correlation)) +
     geom_bar(stat = "identity", fill = "skyblue") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
     labs(title = "Correlation between numerical features and Sales Price",
          x = "Numerical Features",
          y = "Correlation with SalePrice")
#subclass, 
#internal: 
# external:
# location: 

train <- train[,c(names(correlation[abs(correlation) < 0.25]),"SalePrice")]


ggplot(train, aes(x = LotArea, y = SalePrice)) +
     geom_boxplot() +
     labs(title = "Boxplot of SalePrice by MSSubClass")

set.seed(20)
isTraining = runif(nrow(train))<.8
trainingData = subset(train,isTraining)
validationData = subset(train,!isTraining)

getHousePriceRMSE <- function(thisModel){
     mean((predict(thisModel,test) - test$card)^2)
}

str(train)

getHousePriceRMSE(lm(SalePrice~.,data=trainingData))
getHousePriceRMSE(lm(y~.^2,data=trainingData))

library(randomForest)

library(ggplot2)
ggplot(train, aes(x = SalePrice)) +
     geom_boxplot() +
     labs(title = "Boxplot of SalePrice",
          x = "SalePrice") +
     theme_minimal() +
     theme(plot.title = element_text(hjust = 0.5)) 

