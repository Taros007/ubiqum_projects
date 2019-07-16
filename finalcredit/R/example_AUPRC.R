
library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
library(PRROC) # for Precision-Recall curve calculations

set.seed(2969)

imbal_train <- twoClassSim(5000,
                           intercept = -25,
                           linearVars = 20,
                           noiseVars = 10)

imbal_test  <- twoClassSim(5000,
                           intercept = -25,
                           linearVars = 20,
                           noiseVars = 10)

# Set up control function for training

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Build a standard classifier using a gradient boosted machine

set.seed(5627)

orig_fit <- train(Class ~ .,
                  data = imbal_train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)


# Create model weights (they sum to one)

model_weights <- ifelse(imbal_train$Class == "Class1",
                        (1/table(imbal_train$Class)[1]) * 0.5,
                        (1/table(imbal_train$Class)[2]) * 0.5)

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

weighted_fit <- train(Class ~ .,
                      data = imbal_train,
                      method = "gbm",
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)

ctrl$sampling <- "down"

down_fit <- train(Class ~ .,
                  data = imbal_train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

ctrl$sampling <- "up"

up_fit <- train(Class ~ .,
                data = imbal_train,
                method = "gbm",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl)

ctrl$sampling <- "smote"

smote_fit <- train(Class ~ .,
                   data = imbal_train,
                   method = "gbm",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)

# Examine results for test set

model_list <- list(original = orig_fit,
                   weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

calc_auprc <- function(model, data){
  index_class2 <- data$Class == "Class2"
  index_class1 <- data$Class == "Class1"
  
  predictions <- predict(model, data, type = "prob")
  
  pr.curve(predictions$Class2[index_class2], predictions$Class2[index_class1], curve = TRUE)
}

# Get results for all 5 models

model_list_pr <- model_list %>%
  map(calc_auprc, data = imbal_test)

model_list_pr %>%
  map(function(the_mod) the_mod$auc.integral)

## $original
## [1] 0.5271963
## 
## $weighted
## [1] 0.6463645
## 
## $down
## [1] 0.6293265
## 
## $up
## [1] 0.6459151
## 
## $SMOTE
## [1] 0.6195916



# Plot the AUPRC curve for all 5 models

results_list_pr <- list(NA)
num_mod <- 1

for(the_pr in model_list_pr){
  
  results_list_pr[[num_mod]] <- data_frame(recall = the_pr$curve[, 1],
                                           precision = the_pr$curve[, 2],
                                           model = names(model_list_pr)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_pr <- bind_rows(results_list_pr)

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = recall, y = precision, group = model), data = results_df_pr) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = sum(imbal_test$Class == "Class2")/nrow(imbal_test),
              slope = 0, color = "gray", size = 1) +
  theme_bw()



auprcSummary <- function(data, lev = NULL, model = NULL){
  
  index_class2 <- data$obs == "Class2"
  index_class1 <- data$obs == "Class1"
  
  the_curve <- pr.curve(data$Class2[index_class2], data$Class2[index_class1], curve = FALSE)
  out <- the_curve$auc.integral
  names(out) <- "AUPRC"
  
  out
  
}

# Re-initialize control function to remove smote and
# include our new summary function

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = auprcSummary,
                     classProbs = TRUE,
                     seeds = orig_fit$control$seeds)

orig_pr <- train(Class ~ .,
                 data = imbal_train,
                 method = "gbm",
                 verbose = FALSE,
                 metric = "AUPRC",
                 trControl = ctrl)

# Get results for auprc on the test set

orig_fit_test <- orig_fit %>%
  calc_auprc(data = imbal_test) %>%
  (function(the_mod) the_mod$auc.integral)

orig_pr_test <- orig_pr %>%
  calc_auprc(data = imbal_test) %>%
  (function(the_mod) the_mod$auc.integral)

# The test errors are the same

identical(orig_fit_test,
          orig_pr_test)

## [1] TRUE

# Because both chose the same
# hyperparameter combination

identical(orig_fit$bestTune,
          orig_pr$bestTune)

#TRUE
