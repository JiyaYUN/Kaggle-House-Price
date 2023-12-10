# Data Wrangling of "Exterior2nd", "MasVnrType", "MasVnrArea", "ExterQual", "ExterCond", "Foundation", "BsmtQual", "BsmtCond" 
###GIA###
remove(list=ls())
setwd("D:/Simon.UR/Fall A/MGC461 Professional Communication")
test <- read.csv("test.csv")
train <- read.csv("train.csv")

library(dplyr)

#Check Percentage of Null Values
NullCheck <- data.frame(
     Num_of_Null = colSums(is.na(train)),
     Perc_of_Null = round(colSums(is.na(train)) / nrow(train) * 100, 4)
)
row.names(NullCheck) <- names(train)
NullCheck <- NullCheck[order(-NullCheck$Perc_of_Null), ]

NullCheck[c("Exterior2nd", "MasVnrType","MasVnrArea","ExterQual","ExterCond",
            "Foundation", "BsmtQual", "BsmtCond"), ]

summary(train[, c("Exterior2nd", "MasVnrType", "MasVnrArea", "ExterQual", "ExterCond", "Foundation", "BsmtQual", "BsmtCond")])

impute_missing_mode_mean <- function(dt) {
     impute_cols <- list(
          'MasVnrType' = 'None',
          'MasVnrArea' = ifelse(dt$MasVnrArea == 'None', 0, as.numeric(dt$MasVnrArea)),
          'BsmtQual' = 'No Basement',
          'BsmtCond' = 'No Basement'
     )
     
     for (col in names(impute_cols)) {
          dt[[col]][is.na(dt[[col]])] <- impute_cols[[col]]
     }
     
     return(dt)
}
train <- impute_missing_mode_mean(train)
colSums(is.na(train[, c("Exterior2nd", "MasVnrType", "MasVnrArea", "ExterQual", "ExterCond", "Foundation", "BsmtQual", "BsmtCond")]))


install.packages(c("caret", "tidyverse"))
library(caret)
library(tidyverse)

summary(train)

summary(lm(SalePrice ~ Exterior2nd + MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + BsmtQual + BsmtCond, data = train))

#Data Wrangling of "SalePrice","GarageCond", "PavedDrive", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea"
###Ziting###
task_EDA <- train[, c("SalePrice","GarageCond", "PavedDrive", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea")]
train$GarageCond
str(train)
task_EDA$PavedDrive
# Data cleaning
# Example: Processing missing values, here fill NA as an example, the specific method depends on the data situation
task_EDA <- task_EDA %>% 
  mutate(
    SalePrice = ifelse(is.na(SalePrice), 0, SalePrice),
    GarageCond = ifelse(is.na(GarageCond), "None", GarageCond),
    PavedDrive = ifelse(is.na(PavedDrive), "None", PavedDrive),
    WoodDeckSF = ifelse(is.na(WoodDeckSF), 0, WoodDeckSF),
    OpenPorchSF = ifelse(is.na(OpenPorchSF), 0, OpenPorchSF),
    EnclosedPorch = ifelse(is.na(EnclosedPorch), 0, EnclosedPorch),
    X3SsnPorch = ifelse(is.na(X3SsnPorch), 0, X3SsnPorch),
    ScreenPorch = ifelse(is.na(ScreenPorch), 0, ScreenPorch),
    PoolArea = ifelse(is.na(PoolArea), 0, PoolArea)
  )

#Check the cleaning data
str(task_EDA)

#According to the data attributes, Garagecond and PavedDrive two groups of character data are encoded and converted
#Code from ChatGPT：https://chat.openai.com/share/3e284d2c-8236-48bd-a7e6-04db06b5970f
#1.Process the Gargecond value: sort it first, then sort it

#Create a binary feature with or without a garage
task_EDA$HasGarage <- ifelse(task_EDA$GarageCond == "None", 0, 1)

#Code situations with garages in order
garageCondLevels <- setNames(c(5, 4, 3, 2, 1), c("Ex", "Gd", "TA", "Fa", "Po"))
task_EDA$GarageCondNum <- garageCondLevels[task_EDA$GarageCond]

#2.Encode and process PavedDrive data
# Determine the coding logic for Y, P, and N
# Use the lm() function to explore the effect of PavedDrive on SalePrice to determine the specific intervals of N, Y, and P values
model_PD <- lm(SalePrice ~ PavedDrive, data = task_EDA)
summary(model_PD)


#Defined sequential mapping
pavedDriveLevels <- setNames(c(3, 1, 0), c("Y", "P", "N"))

#Apply 
task_EDA$PavedDriveNum <- pavedDriveLevels[task_EDA$PavedDrive]

Summary(lm(SalePrice ~ GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea,data = task_EDA))
