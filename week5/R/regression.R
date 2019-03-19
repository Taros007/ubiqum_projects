## Multiple regression - week 5
## Toine - March 2019

## Load libraries =================================
library(tidyverse)
library(caret)
library(e1071)
library(magrittr)

## Import dataset =================================
#existingProducts <- readr::read_csv('./input/existingproductattributes2017.csv')
#newProducts <- readr::read_csv('./input/newproductattributes2017.csv')

#Use read_delim for semi-colon delimited files and specification of ,.
#dftemp <- readr::read_delim('./input/existingChristianProud.csv',
#                                      delim = ";",
#                                      col_types = cols(.default = col_character()))
#dftemp$Depth <- gsub("\\.", ",", dftemp$Depth)

#existingProducts <- readr::type_convert(dftemp, locale = locale(decimal_mark = ","))
existingProducts <- readr::read_csv2('./input/existingChristianProud.csv')# newProducts <- readr::read_csv('./input/newproductChristianProud.csv')

## Preprocessing: cleaning up ==========================
existingProducts %<>% select(-X1)
names(existingProducts) %<>% make.names(.)

## Preprocessing: alter datatypes & calculate new variables ===============
existingProducts %<>%
  mutate(
    Product_type = as.factor(Product_type),
    Depth = as.numeric(Depth),
    Age = as.factor(Age),
    Review_index = (5 * X5Stars + 4 * X4Stars + 3 * X3Stars + 2 * X2Stars + X1Stars) / rowSums(select(existingProducts, X5Stars:X1Stars))
    ) 

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = existingProducts)
existingDummy <- data.frame(predict(newDataFrame, newdata = existingProducts))

## Missing data =================================

existingDummy <- na.omit(existingDummy)

## Feature selection =================================

existingDummySelected <- select(existingDummy,
                          -c(
                             X5Stars, 
                             X4Stars, 
                             X3Stars, 
                             X2Stars, 
                             X1Stars, 
                             Depth, 
                             Weigth, 
                             Width, 
                             Heigth))

## Training of model =================================
set.seed(998)
# train and test
train_ids <- createDataPartition(y = existingDummy$Volume,
                                 p = 0.75,
                                 list = F)
train <- existingDummySelected[train_ids,]
test <- existingDummySelected[-train_ids,]

# cross validation
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5
                     )


#train Random Forest Regression model
rfFit1 <- caret::train(Volume~. ,
                data = train,
                method = "rf",
                trControl=ctrl
                )

# Predicting testset ================================

#Predict
predictions <- predict(rfFit1, test)

postResample(predictions, test$Volume)

#Check important variables
varTun <- varImp(rfFit1)
plot(varTun, main = "Top variance importance")
