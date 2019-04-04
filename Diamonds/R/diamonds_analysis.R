# Libraries ----
library(tidyverse)
library(magrittr)
library(doParallel)
library(corrplot)
library(GGally)
library(caret)

# Prepare clusters =================================
cl <- makeCluster(3)
registerDoParallel(cl)

# import data 
knownDiamonds <- readRDS('./input/train.rds')

# Dataset information ----
?diamonds

# Data insights ----

# The diamonds with a bad cut are in average more expensive
knownDiamonds %>% 
  ggplot(aes(cut, price)) + 
    geom_boxplot()

# The diamonds with a bad color are also more expensive
knownDiamonds %>% 
  ggplot(aes(color, price)) + 
    geom_boxplot()

# And the diamonds with a bad clarity have a higer price
knownDiamonds %>% 
  ggplot(aes(clarity, price)) +
    geom_boxplot()

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

#Explore data
glimpse(knownDiamonds)
summary(knownDiamonds)

#Find NAs - none found
sapply(knownDiamonds, function(x) sum(is.na(x)))

#plotting all numeric variables
knownDiamonds %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

## Detect collinearity & correlation =========================
corrData <- cor(knownDiamonds %>% select(-cut,-color, -clarity, -id))
corrplot(corrData, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         diag = FALSE)

## Investigate relation between caret, cut, and price ========
knownDiamonds %>% 
  select("cut", "price", "carat") %>% 
  GGally::ggpairs()

# How higher the quality, how lighter (smaller) the diamond
ggplot(knownDiamonds) + geom_boxplot(mapping = aes(x = cut, y = carat)) + coord_flip()
# How better the color, how lighter (smaller) the diamond
ggplot(knownDiamonds) + geom_boxplot(mapping = aes(x = color, y = carat)) + coord_flip()

# Any combined relationship?
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(knownDiamonds, aes(x = carat, y = price, color = cut)) + 
  geom_point() +
  scale_color_manual(values = cbbPalette)

## Create sample of data for quick testing =======
knownDiamonds %<>% sample_frac(0.1)

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = knownDiamonds)
diamondDummy <- data.frame(predict(newDataFrame, newdata = knownDiamonds))

#Remove features
diamondDummy %<>% select(-depth, -x, -y, -z,-id) #

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
                         method = "rf",
                         trControl=ctrl
                         #preProcess = c("scale", "center") #only used for distance-modelling techniques (knn, SVM)
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

saveRDS(finalPredictions, './output/predToine1.rds')
