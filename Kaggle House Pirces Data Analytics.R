rm(list=ls())

setwd("D:/Simon.UR/Fall A/MGC461 Professional Communication")
train <- read.csv("train.csv")
train <- read.csv("train.csv")

library(dplyr)





### DATA CLEANING: DEAL WITH NA ###

#find out columns that include NA => 19 of all
missing_columns <- colnames(train)[colSums(is.na(train)) > 0]
print(missing_columns)

#1460 rows with NA => cannot simply delete rows with NA
nrow(train[!complete.cases(train),])

#duplicate a train dataset
train_new <- train

#regression imputation of NAs in LotFrontage：
model_LotFrontage <- lm(LotFrontage ~ LotArea + Street + Neighborhood + LotConfig, 
                     data = train_new, 
                     na.action = na.exclude)
missing_values_LotFrontage <- train_new[is.na(train_new$LotFrontage), ]
predicted_values_LotFrontage <- predict(model_LotFrontage, newdata = missing_values_LotFrontage)
train_new$LotFrontage[is.na(train_new$LotFrontage)] <- predicted_values_LotFrontage

#delete columns Alley and GarageYrBlt (don't know how to deal with NAs in Year properly)
train_new <- train_new[ , !(names(train_new) %in% c("Alley", "GarageYrBlt"))]

#only 1 NA in Electrical => Make it 'SBrkr' (the mode)
train_new$Electrical[is.na(train_new$Electrical)] <- "SBrkr"

#make NAs in columns below into 'None'
#columns: MasVnrType, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu, GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature
cols_to_replace <- c("MasVnrType", "BsmtQual", "BsmtCond", "BsmtExposure", 
                     "BsmtFinType1", "BsmtFinType2", "FireplaceQu", 
                     "GarageType", "GarageFinish", "GarageQual", 
                     "GarageCond", "PoolQC", "Fence", "MiscFeature")
for (col in cols_to_replace) {
    train_new[[col]][is.na(train_new[[col]])] <- "None"
}

#make NAs in column MasVnrArea into 0
train_new$MasVnrArea[is.na(train_new$MasVnrArea)] <- 0

#transfer character to factor
train_new <- train_new %>%
    mutate(
        MSSubClass = as.factor(MSSubClass),
        MSZoning = as.factor(MSZoning),
        Street = as.factor(Street),
#        Alley = as.factor(Alley),
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

#shrink price unit to thousand
train_new$SalePrice=train_new$SalePrice/100

### CASUAL REGRESSION ###

#run lm model to see causal relationship

options(max.print=10000)

#simple one
model1=lm(SalePrice~.-Id,data=train_new)
summary(model1)

#do log on SalePrice
model2=lm(log(SalePrice)~.-Id,data=train_new)
summary(model2)
summary(train_new)
