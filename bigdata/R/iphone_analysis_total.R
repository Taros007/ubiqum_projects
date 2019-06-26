# Load libraries ----------------------------------------------------------
library(tidyverse)
library(caret)
library(magrittr)
library(doParallel)
library(DMwR)
source('./R/run_model.R')

# Load data ---------------------------------------------------------------
iphoneData <- read_csv('./input/iphone_smallmatrix_labeled_8d.csv')

# Select variables --------------------------------------------------------
iphoneCS <- iphoneData %>%
  select(c("iphone",
           #"ios",
           "iphonecampos",
           "iphonecamneg", 
           "iphonecamunc", 
           "iphonedispos", 
           "iphonedisneg",
           "iphonedisunc",
           "iphoneperpos",
           "iphoneperneg",
           "iphoneperunc",
           #"iosperpos",
           #"iosperneg",
           #"iosperunc",
           "iphonesentiment")
  )

#iphoneCS %<>% filter(iphone != 41)
#iphoneCS %<>% distinct_all()
iphoneCS %<>% mutate(
  iphonesentiment = factor(iphonesentiment)
  )

iphoneCS$sum <- rowSums(iphoneCS %>% select(-c(iphone, iphonesentiment)))
iphoneCS %<>% filter(sum != 0) %>% select(-"sum")

#ovun.sample(iphonesentiment ~ ., data = iphoneCS, method="both", N, p=0.5, seed = 3)
#iphoneCS <- upSample(iphoneCS, iphoneCS$iphonesentiment)

set.seed(107)

dataset <- iphoneCS
modeltype <- "ranger"
dependant <- "iphonesentiment"

# train and test
train_ids <- createDataPartition(y = dataset[[dependant]],
                                 p = 0.80,
                                 list = F)
train <- dataset[train_ids,]
test <- dataset[-train_ids,]

model <- run_model(train, test, modeltype, dependant)
model$model

confusionMatrix(data = predict(model$model, newdata = test),
                reference = test$iphonesentiment,
                dnn = c("Prediction","Reference"))
                