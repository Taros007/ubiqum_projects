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

## Create sample of data for quick testing =======
#knownDiamonds %<>% sample_frac(0.1)

for (i in c("Ideal","Premium", "Very Good", "Good", "Fair")){
  selectedDiamonds <- filter(knownDiamonds, cut == i)
  selectedDiamonds %<>% select(-depth, -y, -x, -z, -id, -cut) 
  newDataFrame <- dummyVars(" ~ .", data = selectedDiamonds)
  selectedDiamonds <- data.frame(predict(newDataFrame, newdata = selectedDiamonds))

  # train and test
  train_ids <- createDataPartition(y = selectedDiamonds$price,
                                   p = 0.75,
                                   list = F)
  train <- selectedDiamonds[train_ids,]
  test <- selectedDiamonds[-train_ids,]
  
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
  
  assign(paste0("rf_",i), rfFit1)
  
  cat("Test", paste0("rf_",i), "results in the following metrics:", postResample(test$Predictions, test$price),"\n")
}

# #Predictions
unknownDiamonds <- readRDS('./input/validation_NOprice.rds')

unknownDiamonds %<>% 
   mutate(
     cut = ordered(as.factor(cut), levels = c("Ideal","Premium", "Very Good", "Good", "Fair")),
     color = as.factor(color),
     clarity = ordered(as.factor(clarity), levels = c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1"))
   )

for (i in c("Ideal","Premium", "Very Good", "Good", "Fair")){
  selecteduDiamonds <- filter(unknownDiamonds, cut == i)
  selecteduDiamonds %<>% select(-depth, -y, -x, -z, -cut)
  newDataFrame <- dummyVars(" ~ .", data = selecteduDiamonds)
  selecteduDiamonds <- data.frame(predict(newDataFrame, newdata = selecteduDiamonds))

  if (i == "Ideal"){
    selecteduDiamonds$pred <- predict(rf_Ideal, selecteduDiamonds)
    finalpredictions <- selecteduDiamonds %>% filter(id<0)
    finalpredictions <- rbind(finalpredictions, selecteduDiamonds)
  }
  if (i == "Premium"){
    selecteduDiamonds$pred <- predict(rf_Premium, selecteduDiamonds)
    finalpredictions <- rbind(finalpredictions, selecteduDiamonds)
  }
  if (i == "Very Good"){
    selecteduDiamonds$pred <- predict(`rf_Very Good`, selecteduDiamonds)
    finalpredictions <- rbind(finalpredictions, selecteduDiamonds)
  }
  if (i == "Good"){
    selecteduDiamonds$pred <- predict(rf_Good, selecteduDiamonds)
    finalpredictions <- rbind(finalpredictions, selecteduDiamonds)
  }
  if (i == "Fair"){
    selecteduDiamonds$pred <- predict(rf_Fair, selecteduDiamonds)
    finalpredictions <- rbind(finalpredictions, selecteduDiamonds)
  }
}

finalpredictions %<>% select("id", "pred")
pred1 <- readRDS('./output/predToine1.rds')

postResample(finalpredictions$pred, pred1$Prediction)

saveRDS(finalpredictions, './output/predToine2.rds')
