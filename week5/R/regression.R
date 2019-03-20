## Multiple regression - week 5
## Toine - March 2019

## Task list =====================================
#Now NAs get deleted, limiting dataset. Test other measures (knnimpute, bagimpute etc.)

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

## Data exploration ==========================================
#plotting dependent variable
#ggplot(existingProducts, aes(x = Product_type, y = Volume)) + geom_boxplot() + coord_flip()
#plotting dependent variable vs most important independent variable (varImp)
ggplot(existingProducts, aes(x = Positive_service_review, y = Volume)) + geom_point()

## Outlier detection $ removal ===============================
source('./R/outliers.R')

#Detect outliers based on MAD
is_no_outlier <- isnt_out_maha(existingProducts$Volume)

# add a column with info whether the Volume is an outlier
existingProducts$is_no_outlier <- is_no_outlier

# look at the same plot as above, with and without outliers
g_withoutliers <- ggplot(existingProducts, aes(Product_type, Volume)) +
                    geom_boxplot() +
                    coord_flip() +
                    geom_boxplot()

g_withoutoutliers <- ggplot(existingProducts[is_no_outlier == T,], aes(Product_type, Volume)) +
                    geom_boxplot() +
                    coord_flip() +
                    geom_boxplot()

plot_grid(g_withoutliers, g_withoutoutliers, labels = c("With outliers", "Without outliers"))

existingProducts <- filter(existingProducts, is_no_outlier == T)

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = existingProducts)
existingDummy <- data.frame(predict(newDataFrame, newdata = existingProducts))

## Feature selection =================================
existingDummySelected <- select(existingDummy,
                           -c(
                             X5Stars,
                             X4Stars,
                             X3Stars,
                             X2Stars,
                             X1Stars,
                             Product_ID,
                             Depth,
                             Weigth,
                             Width,
                             Heigth
                             ))

## Missing data =================================
#after feature selection to retain as much data as possible
existingDummySelected <- na.omit(existingDummySelected)

## Detect collinearity & correlation =========================
corrData <- cor(existingDummySelected)
corrplot(corrData, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         diag = FALSE)

## Training of model =================================
set.seed(998)
# train and test
train_ids <- createDataPartition(y = existingDummySelected$Volume,
                                 p = 0.75,
                                 list = F)
train <- existingDummySelected[train_ids,]
test <- existingDummySelected[-train_ids,]

# cross validation
ctrl <- trainControl(method = "repeatedcv",
                     number = 3,
                     repeats = 3
                     )


#train Random Forest Regression model
rfFit1 <- caret::train(Volume~. ,
                data = train,
                method = "rf",
                trControl=ctrl,
                importance=T #added to allow for varImp()
                )

# Predicting testset ================================
#Predict
predictions <- predict(rfFit1, test)

postResample(predictions, test$Volume)

#Check important variables
varTun <- varImp(rfFit1)
plot(varTun, main = "Top variance importance")

# Closing actions ================================

#Save predictions
#write.csv(surveyIncom, './output/SurveyIncompletePredicted.csv')
#write.csv(surveyData, './output/SurveyData.csv')

# Stop Cluster. 
stopCluster(cl)                   
