library(caret)
library(tidyverse)
library(PRROC)

#Script control
relev_vars <- c("TARGET", "REGION_POPULATION_RELATIVE", "AMT_INCOME_TOTAL")
model <- "pls"
preProc <- c("scale", "center")
set.seed(123)

# AUPRC functions ---------------------------------------------------------
calc_auprc <- function(model, data){
  index_class2 <- data$TARGET == levels(data$TARGET)[2]
  index_class1 <- data$TARGET == levels(data$TARGET)[1]
  
  predictions <- predict(model, data, type = "prob")
  
  pr.curve(predictions$Performing[index_class2], predictions$Performing[index_class1], curve = TRUE)
}

auprcSummary <- function(data, lev = NULL, model = NULL){
  index_class2 <- data$obs == levels(data$TARGET)[2]
  index_class1 <- data$obs == levels(data$TARGET)[1]
  
  the_curve <- pr.curve(data$Performing[index_class2], data$Performing[index_class1], curve = FALSE)
  out <- the_curve$auc.integral
  names(out) <- "AUPRC"
  out
}

# Create train and verification sets
train_ids <- createDataPartition(y = application_train$TARGET,
                                 p = 0.75,
                                 list = F)

train <- application_train[train_ids,]
test <- application_train[-train_ids,]

ctrl <- trainControl(
  method = "repeatedcv", 
  number = 1,
  repeats = 3,
  classProbs = TRUE, 
  summaryFunction = prSummary #= twoClassSummary
  )

plsFit <- train(
  TARGET ~ .,
  data = train[relev_vars],
  method = model,
 # preProc = preProc,
  tuneLength = 15,
  trControl = ctrl,
  metric = "AUC"
  )

#plsFit

#postResample(predict(plsFit, test), test$TARGET)

y <- data.frame(obs = factor(test$TARGET), 
                pred = predict(plsFit, test),
                performing = predict(plsFit, test, type = "prob")) %>% 
     rename(default = performing.default,
            performing = performing.performing)

twoClassSummary(data = y, lev = levels(test$TARGET),model = "pls")

#Confusion matrix
confusionMatrix(y$pred, y$obs)

#Create ROC and PR plots
fg <- y$default[y$obs == "default"]
bg <- y$performing[y$obs == "performing"]

# ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)
