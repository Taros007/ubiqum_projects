
# Load libraries ----------------------------------------------------------
library(caret)
library(doParallel)

# Function declaration ----------------------------------------------------

run_knn <- function(modeldata, dependant, p = 0.75) {

  #Load clusters
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
    
  set.seed(621)
  #Create training and testing sets
  train_ids <- createDataPartition(y = modeldata[[dependant]],
                                   p = p,
                                   list = F)
  train <- modeldata[train_ids,]
  test <- modeldata[-train_ids,]
  
  # cross validation
  ctrl <- trainControl(method = "repeatedcv",
                       number = 4,
                       repeats = 1
  )
  
  #train Random Forest Regression model
  rfFit1 <- caret::train(as.formula(paste(dependant, "~ .")),
                         data = train,
                         method = "knn",
                         trControl=ctrl,
                         preProcess = c("scale", "center") #only used for distance-modelling techniques (knn, SVM)
  )
  
  # Predicting testset ================================
  test$Predictions <- predict(rfFit1, test)
  
  # Stop Cluster. 
  stopCluster(cl)
  
  # Return test
  return(list(predictions = test, model = rfFit1))
}
