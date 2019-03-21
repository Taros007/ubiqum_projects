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
existingProducts %<>% select(-X1)
names(existingProducts) %<>% make.names(.)

## Preprocessing: alter datatypes & calculate new variables ===============
existingProducts %<>%
  mutate(
    Product_type = as.factor(Product_type),
    Depth = as.numeric(Depth),
    Age = as.factor(Age),
    Professional = as.factor(Professional),
    Review_score = (5 * X5Stars + 4 * X4Stars + 3 * X3Stars + 2 * X2Stars + X1Stars) / rowSums(select(existingProducts, X5Stars:X1Stars))
  )

## Bin Best_seller_rank, and convert NAs to 0 ================

existingProducts$Best_seller_rank %<>% 
  findInterval(c(-Inf, 50, 100, Inf)) %<>% 
  replace_na(0) %<>% as.factor()

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = existingProducts)
existingDummy <- data.frame(predict(newDataFrame, newdata = existingProducts))

#Load model
#rfFit1 <- readRDS('./output/RF.rds')

# Predicting testset ================================
test$Predictions <- predict(rfFit1, test)
postResample(test$Predictions, test$Volume)

#Save predictions
write.csv(surveyIncom, './output/SurveyIncompletePredicted.csv')
