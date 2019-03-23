## Predict new sales volumes - week 5
## Toine - March 2019

## Load libraries =================================
library(tidyverse)
library(caret)
library(e1071)
library(magrittr)
library(doParallel)
library(corrplot)
library(cowplot)

# Prepare clusters =================================
cl <- makeCluster(3)
registerDoParallel(cl)

## Import dataset =================================
newProducts <- readr::read_csv2('./input/newproductChristianProud.csv')

## Preprocessing: cleaning up ==========================
newProducts %<>% select(-X1)
names(newProducts) %<>% make.names(.)
names(newProducts$Age_group) <- "Age"

## Preprocessing: alter datatypes & calculate new variables ===============
newProducts %<>%
  mutate(
    Product_type = as.factor(Product_type),
    Depth = as.numeric(Depth),
    Age = as.factor(Age),
    Professional = as.factor(Professional),
    Review_score = (4 * X4Stars + 3 * X3Stars + 2 * X2Stars + X1Stars) / rowSums(select(newProducts, X4Stars:X1Stars))
  )

## Bin Best_seller_rank, and convert NAs to 0 ================

newProducts$Best_seller_rank %<>% 
  findInterval(c(-Inf, 50, 100, Inf)) %<>% 
  replace_na(0) %<>% as.factor()

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = newProducts)
newDummy <- data.frame(predict(newDataFrame, newdata = newProducts))

#Load model
rfFit1 <- readRDS('./output/RF.rds')

# Predicting testset ================================
existingDummy$Volume <- predict(rfFit1, newDummy)

#Save predictions
write.csv(surveyIncom, './output/Predicted.csv')
