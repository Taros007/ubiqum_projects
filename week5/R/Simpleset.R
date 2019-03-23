## Multiple regression - week 5
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
#existingProducts <- readr::read_csv('./input/existingproductattributes2017.csv')
#newProducts <- readr::read_csv('./input/newproductattributes2017.csv')

existingProducts <- readr::read_csv('./input/existingproductattributes2017.csv')

## Preprocessing: cleaning up ==========================
#existingProducts %<>% select(-X1)
names(existingProducts) %<>% make.names(.)

## Preprocessing: alter datatypes & calculate new variables ===============
existingProducts %<>%
  mutate(
    ProductType = as.factor(ProductType),
    #Depth = as.numeric(Depth),
    Review_score = (4 * x4StarReviews + 3 * x3StarReviews + 2 * x2StarReviews + x1StarReviews) / rowSums(select(existingProducts, x4StarReviews:x1StarReviews))
  )
existingProducts %<>% filter(Volume>0)


## Data exploration ==========================================
#plotting dependent variable
ggplot(existingProducts, aes(x = ProductType, y = Volume)) +
  geom_boxplot() +
  coord_flip()

#plotting all numeric variables
existingProducts %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#plotting dependent variable vs most important independent variable (varImp)
ggplot(existingProducts, aes(x = Recommendproduct, y = Volume)) + geom_point()

## Outlier detection & removal ===============================
source('./R/outliers.R')

#Detect outliers based on MAD
is_no_outlier <- isnt_out_mad(existingProducts$Volume, thres = 14)
# add a column with info whether the Volume is an outlier
existingProducts$is_no_outlier <- is_no_outlier

# look at the same plot as above, with and without outliers
g_withoutliers <- ggplot(existingProducts, aes(ProductType, Volume)) +
  geom_boxplot() +
  coord_flip() +
  geom_boxplot()

g_withoutoutliers <- ggplot(existingProducts[is_no_outlier == T,], aes(ProductType, Volume)) +
  geom_boxplot() +
  coord_flip() +
  geom_boxplot()

plot_grid(g_withoutliers, g_withoutoutliers, labels = c("With outliers", "Without outliers"))

existingProducts <- existingProducts[is_no_outlier,]

## Detect collinearity & correlation =========================
corrData <- cor(existingProducts %>% select(-ProductType) %>% na.omit())
corrplot(corrData, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         diag = FALSE)

## Bin Best_seller_rank, and convert NAs to 0 ================

existingProducts$BestSellersRank %<>% 
  findInterval(c(-Inf, 50, 100, Inf)) %<>% 
  replace_na(0) %<>% as.factor()

## Feature selection =================================
existingSelected <- select(existingProducts,
                           -c(
                             x5StarReviews,
                             x4StarReviews,
                             x3StarReviews,
                             x2StarReviews,
                             x1StarReviews,
                             #Review_score,
                             #PositiveServiceReview,
                             ProductNum,
                             ProductDepth,
                             ShippingWeight,
                             ProductWidth,
                             ProductHeight,
                             #Price,
                             BestSellersRank,
                             is_no_outlier
                             #Recommendproduct
                             ))

#existingSelected <- existingProducts %>% 
# select(X4Stars,X2Stars....) Pericles

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = existingSelected)
existingDummy <- data.frame(predict(newDataFrame, newdata = existingSelected))

## Missing data =================================
#after feature selection to retain as much data as possible
existingDummy <- na.omit(existingDummy)

## Training of model =================================
for (n in 1:100) {
  set.seed(n)
  # train and test
  train_ids <- createDataPartition(y = existingDummy$Volume,
                                   p = 0.75,
                                   list = F)
  train <- existingDummy[train_ids,]
  test <- existingDummy[-train_ids,]
  
  # cross validation
  ctrl <- trainControl(method = "repeatedcv",
                       number = 4,
                       repeats = 10
  )
  
  #train Random Forest Regression model
  rfFit1 <- caret::train(Volume~. ,
                         data = train,
                         preProcess= c("center"),
                         method = "rf",
                         trControl=ctrl,
                         importance=T #added to allow for varImp()
  )
  
  # Predicting testset ================================
  test$Predictions <- predict(rfFit1, test)
  postResample(test$Predictions, test$Volume)
  
  cat("Test", n, "results in", postResample(test$Predictions, test$Volume),"\n")
  }

ggplot(test, aes(x = Volume, y = Predictions)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

ggplot(filter(test, ProductType.Laptop == 1), aes(x = Volume, y = Predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

#Check important variables
varTun <- varImp(rfFit1)
plot(varTun, main = "Top variance importance")

# Closing actions ================================

#Save model to avoid future retraining
#saveRDS(rfFit1, './output/RF.rds')

# Stop Cluster. 
stopCluster(cl)                   
