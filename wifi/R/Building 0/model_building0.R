
# Load data ---------------------------------------------------------------
source('./R/data_loading.R')
source('./R/data_loading_verification.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)

# Select building 0 observations ------------------------------------------

wifiData %<>% filter(wifiData$BUILDINGID == 0)
wifiVerification %<>% filter(wifiVerification$BUILDINGID == 0)

# Adjust variables --------------------------------------------------------
wifiData %<>% mutate(
  FLOOR = as.factor(FLOOR),
  BUILDINGID = as.factor(BUILDINGID),
  SPACEID = as.factor(SPACEID),
  RRELATIVEPOSITION = as.factor(RELATIVEPOSITION),
  USERID = as.factor(USERID),
  PHONEID = as.factor(PHONEID),
  TIMESTAMP = as_datetime(TIMESTAMP, tz = "Europe/Madrid")
  )

# Remove variables with variance 0 ----------------------------------------
constantVars <- which(apply(wifiData, 2, var)==0)

if(length(constantVars)>0){
  wifiData <- wifiData[,-constantVars]  
  wifiVerification <- wifiVerification[,-constantVars] 
}

# Check signal strength per user ------------------------------------------
# 
# wifiData %>%
#   select(c(contains("WAP"), "USERID")) %>%
#   gather(key = "WAP", value = "Signal", -USERID) %>%
#   ggplot(aes(x = USERID, y = Signal, color = USERID)) +
#   geom_jitter() +
#   geom_violin(scale = 'area', alpha = 0.7, fill = '#808000') +
#   labs(title = 'Explore signal strength per user') +
#   xlab('')

# Check observations over time --------------------------------------------
# 
# wifiData %>% 
#   ggplot(aes(x = TIMESTAMP, fill = USERID)) +
#   geom_histogram(binwidth = 20)
  
# Create initial baseline models -----------------------------------------------

#Create training and testing sets
set.seed(541)
train_ids <- sample(seq_len(nrow(wifiData)), size = floor(0.75 * nrow(wifiData)))

train <- wifiData[train_ids,]
test <- wifiData[-train_ids,]

source('./R/run_model.R')

# FLOOR model -------------------------------------------------------------
dependant = "LATITUDE"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, "knn", dependant)
postResample(results$predictions, pull(testData[,c(dependant)]))

# Without mutate_at
# Accuracy  Kappa 
# 0.7651822 0.6943330

# With mutate_at
# Accuracy  Kappa 
# 0.8461538 0.8000937 

# Verification data
wifiVerification$Predictions <- predict(test$model, select(wifiVerification, c(contains("WAP"))))
postResample(wifiVerification$Predictions, wifiVerification$FLOOR)

# With mutate_at
# Accuracy  Kappa 
# 0.7488749 0.6502643 

# LAT model ---------------------------------------------------------------
modelData <- select(wifiData, c(contains("WAP"), LATITUDE))
test <- run_knn(modelData, "knn", "LATITUDE")
postResample(test$predictions$Predictions, test$predictions$LATITUDE)
coordinates <- data.frame(LATpred = test$predictions$Predictions, LATact = test$predictions$LATITUDE)

# Without mutate_at
# RMSE        Rsquared   MAE 
# 13.9494770  0.9599392  9.1303813

# With mutate_at
# RMSE        Rsquared   MAE 
# 15.3765811  0.9518108  8.7828804 

# Verification data
wifiVerification$Predictions <- predict(test$model, select(wifiVerification, c(contains("WAP"))))
postResample(wifiVerification$Predictions, wifiVerification$LATITUDE)

# With mutate_at
# RMSE        Rsquared  MAE 
# 19.5894360  0.9228687 11.0990749 

# LNG model ---------------------------------------------------------------
modelData <- select(wifiData, c(contains("WAP"), LONGITUDE))
test <- run_knn(modelData, "knn", "LONGITUDE")
postResample(test$predictions$Predictions, test$predictions$LONGITUDE)
coordinates <- cbind(coordinates, LNGpred = test$predictions$Predictions, LNGact = test$predictions$LONGITUDE)

# Without mutate_at
# RMSE        Rsquared   MAE 
# 15.2954115  0.9850709  9.6400422 

# With mutate_at
# RMSE        Rsquared   MAE 
# 16.8034956  0.9819268  8.2522013 

# Verification data
wifiVerification$Predictions <- predict(test$model, select(wifiVerification, c(contains("WAP"))))
postResample(wifiVerification$Predictions, wifiVerification$LONGITUDE)

# With mutate_at
# RMSE        Rsquared  MAE 
# 34.9828158  0.9183959 15.3340369 


# Calculate distance between actual and pred ------------------------------

coordinates$distance <- sqrt(
  (coordinates$LATpred - coordinates$LATact)^2 + 
  (coordinates$LNGpred - coordinates$LNGact)^2
  )

coordinates %>% 
  ggplot(aes(x=distance)) + 
  geom_density()