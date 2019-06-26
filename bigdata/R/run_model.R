
# Load libraries ----------------------------------------------------------
library(caret)
library(doParallel)

# Function declaration ----------------------------------------------------

run_model <- function(traindata, testdata, model, dependant) {

  #Load clusters
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
    
  set.seed(621)

  # cross validation
  ctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 1,
                       sampling = "smote"
  )
  
  #train Random Forest Regression model
  rfFit1 <- caret::train(as.formula(paste(dependant, "~ .")),
                         data = traindata,
                         method = model,
                         #metric = "kappa",
                         trControl=ctrl
                        # preProcess = c("zv")
  )
  
  # Predicting testset ================================
  testdata$predictions <- predict(rfFit1, testdata)
  
  # Stop Cluster. 
  stopCluster(cl)
  
  # Return test
  return(list(predictions = testdata$predictions, model = rfFit1))
}
