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

existingProducts <- readr::read_csv2('./input/existingChristianProud.csv')
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
existingProducts %<>% filter(Volume>0)


## Data exploration ==========================================
#plotting dependent variable
ggplot(existingProducts, aes(x = Product_type, y = Volume)) +
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
ggplot(existingProducts, aes(x = Positive_service_review, y = Volume)) + geom_point()

## Outlier detection & removal ===============================
source('./R/outliers.R')

#Detect outliers based on MAD
is_no_outlier <- isnt_out_mad(existingProducts$Volume, thres = 50)
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

existingProducts <- existingProducts[is_no_outlier,]

## Detect collinearity & correlation =========================
corrData <- cor(existingProducts %>% select(-Age,-Product_type, -Professional) %>% na.omit())
corrplot(corrData, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         diag = FALSE)

## Bin Best_seller_rank, and convert NAs to 0 ================

existingProducts$Best_seller_rank %<>% 
  findInterval(c(-Inf, 50, 100, Inf)) %<>% 
  replace_na(0) %<>% as.factor()

## Feature selection =================================
# existingSelected <- select(existingProducts,
#                            c(
#                              Review_score,
#                              Prices,
#                              Competitors,
#                              Positive_service_review,
#                              Width,
#                              Volume,
#                              Product_type
#                            ))

#existingSelected <- existingProducts %>% 
# select(X4Stars,X2Stars....) Pericles

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = existingProducts)
existingDummy <- data.frame(predict(newDataFrame, newdata = existingProducts))

## Missing data =================================
#after feature selection to retain as much data as possible
existingDummy <- na.omit(existingDummy)

## Training of model =================================
set.seed(541)
# train and test
train_ids <- createDataPartition(y = existingDummy$Product_type.Netbook,
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
rfFit1 <- caret::train(Volume~ Review_score + 
                         Negative_service_review +
                         Positive_service_review +
                         X4Stars +
                         Profit_margin+
                         Would_consumer_recomend__product,
                       data = train,
                       method = "rf",
                       trControl=ctrl,
                       importance=T #added to allow for varImp()
)

# Predicting testset ================================
test$Predictions <- predict(rfFit1, test)
postResample(test$Predictions, test$Volume)

Filtered <- filter(test, Product_type.Netbook == 1)
cat("Amount of Netbooks in testset is:", nrow(Filtered))
postResample(Filtered$Predictions, Filtered$Volume)

ggplot(test, aes(x = Volume, y = Predictions)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

ggplot(filter(test, Product_type.Netbook == 1), aes(x = Volume, y = Predictions)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

#Check important variables
varTun <- varImp(rfFit1)
plot(varTun, main = "Top variance importance")

# Closing actions ================================

# Stop Cluster. 
stopCluster(cl)                   
