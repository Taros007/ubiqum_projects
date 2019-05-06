
# Load data ---------------------------------------------------------------
source('./R/data_loading.R')
source('./R/data_loading_verification.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)

# Remove variables with variance 0 ----------------------------------------
#TODO: is there a logic in how wifi devices are numbered - in case verification data contains removed WAP data
constantVars <- which(apply(wifiData, 2, var)==0)

if(length(constantVars)>0){
  wifiData <- wifiData[,-constantVars]  
  wifiVerification <- wifiVerification[,-constantVars] 
}

# TEMP: throw out data ----------------------------------------------------

## Just one building
#wifiData %<>% filter(BUILDINGID == 0)

## Random subset of data
# take a random sample of size 50 from a dataset mydata
# sample without replacement
set.seed(541)

wifiData <- wifiData[sample(1:nrow(wifiData), 2500,
                          replace=FALSE),]

# Check signal strength per user ------------------------------------------

# wifiData %>%
#   select(c(WAP001:WAP520, USERID)) %>%
#   gather(key = "WAP", value = "Signal", -USERID) %>%
#   ggplot(aes(x = USERID, y = Signal, color = USERID)) +
#   geom_jitter() +
#   geom_violin(scale = 'area', alpha = 0.7, fill = '#808000') +
#   labs(title = 'Explore signal strength per user') +
#   xlab('')


# Create initial baseline models -----------------------------------------------
source('./R/run_knn.R')


# BUILDINGID model --------------------------------------------------------
modelData <- select(wifiData, c(contains("WAP"), BUILDINGID))
test <- run_knn(modelData, "BUILDINGID")
postResample(test$predictions$Predictions, test$predictions$BUILDINGID)

# Without mutate_at
# Accuracy Kappa 
# 1        1 

# With mutate_at
# Accuracy  Kappa 
# 0.9959839 0.9936849

# Verification data
wifiVerification$Predictions <- predict(test$model, select(wifiVerification, c(contains("WAP"))))
postResample(wifiVerification$Predictions, wifiVerification$BUILDINGID)

# With mutate_at
# Accuracy  Kappa 
# 0.9684968 0.9505268 

# FLOOR model -------------------------------------------------------------
modelData <- select(wifiData, c(contains("WAP"), FLOOR))
test <- run_knn(modelData, "FLOOR")
postResample(test$predictions$Predictions, test$predictions$FLOOR)
  
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
test <- run_knn(modelData, "LATITUDE")
postResample(test$predictions$Predictions, test$predictions$LATITUDE)

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
test <- run_knn(modelData, "LONGITUDE")
postResample(test$predictions$Predictions, test$predictions$LONGITUDE)

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
