# Libraries ----
library(tidyverse)
library(magrittr)
library(doParallel)
library(corrplot)
library(GGally)
library(caret)

source('../week5/R/outliers.R')

# Prepare clusters =================================
cl <- makeCluster(3)
registerDoParallel(cl)

# import data 
knownDiamonds <- readRDS('./input/train.rds')

# Your task ----

# Why the diamonds that have a fair cut, bad color and a bad clarity are, in median, more expensive? 
# We would like to receive a model to predict their price and to know which ones are 
# the most relevant features to do that. 

# Wrangle data
knownDiamonds %<>% 
  mutate(
    cut = ordered(as.factor(cut), levels = c("Ideal","Premium", "Very Good", "Good", "Fair")),
    color = as.factor(color),
    clarity = ordered(as.factor(clarity), levels = c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1"))
    )

#Remove outliers
is_no_outlier <- isnt_out_mad(knownDiamonds$price, thres = 3)
knownDiamonds$is_no_outlier <- is_no_outlier
knownDiamonds <- knownDiamonds[is_no_outlier,]
knownDiamonds <- select(knownDiamonds, -is_no_outlier)

is_no_outlier <- isnt_out_mad(knownDiamonds$carat, thres = 3)
knownDiamonds$is_no_outlier <- is_no_outlier
knownDiamonds <- knownDiamonds[is_no_outlier,]
knownDiamonds <- select(knownDiamonds, -is_no_outlier)

is_no_outlier <- isnt_out_mad(knownDiamonds$depth, thres = 3)
knownDiamonds$is_no_outlier <- is_no_outlier
knownDiamonds <- knownDiamonds[is_no_outlier,]
knownDiamonds <- select(knownDiamonds, -is_no_outlier)

knownDiamonds %<>% select(-id) 

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = knownDiamonds)
diamondDummy <- data.frame(predict(newDataFrame, newdata = knownDiamonds))

## Training of model =================================
for (n in 1) {  #only one run due to time constraints.
  set.seed(541)
  # train and test
  train_ids <- createDataPartition(y = diamondDummy$price,
                                   p = 0.75,
                                   list = F)
  train <- diamondDummy[train_ids,]
  test <- diamondDummy[-train_ids,]
  
  # cross validation
  ctrl <- trainControl(method = "repeatedcv",
                       number = 4,
                       repeats = 1
                      )
  
  #train Random Forest Regression model
  rfFit1 <- caret::train(price~ .,
                         data = train,
                         method = "knn",
                         trControl=ctrl,
                         preProcess = c("scale", "center") #only used for distance-modelling techniques (knn, SVM)
                        )
  
  # Predicting testset ================================
  test$Predictions <- predict(rfFit1, test)
  postResample(test$Predictions, test$price)
  
  cat("Test", n, "results in the following metrics:", postResample(test$Predictions, test$price),"\n")
} #End of the for loop to test stability of the model with different seed numbers.

#Plot predicted vs. actual values
ggplot(test, aes(x = price, y = Predictions)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

#Predictions
unknownDiamonds <- readRDS('./input/validation_NOprice.rds')

unknownDiamonds %<>% 
  mutate(
    cut = ordered(as.factor(cut), levels = c("Ideal","Premium", "Very Good", "Good", "Fair")),
    color = as.factor(color),
    clarity = ordered(as.factor(clarity), levels = c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1"))
  )

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = unknownDiamonds)
unknownDiamondsDummy <- data.frame(predict(newDataFrame, newdata = unknownDiamonds))

unknownDiamondsDummy$Prediction <- predict(rfFit1, unknownDiamondsDummy)

finalPredictions <- unknownDiamondsDummy %>% select("id", "Prediction")

pred1 <- readRDS('./output/predToine1.rds')

postResample(finalPredictions$Prediction, pred1$Prediction)

saveRDS(finalPredictions, './output/predToine3.rds')
