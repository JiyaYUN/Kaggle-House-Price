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
