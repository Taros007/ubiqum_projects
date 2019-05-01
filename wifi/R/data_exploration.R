
# Load data ---------------------------------------------------------------
source('./R/data_loading.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)

# TEMP: throw out data ----------------------------------------------------

## Just one building
#wifiData %<>% filter(BUILDINGID == 0)

## Random subset of data
# take a random sample of size 50 from a dataset mydata
# sample without replacement
wifiData <- wifiData[sample(1:nrow(wifiData), 1000,
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
set.seed(541)
source('./R/run_knn.R')

#BUILDINGID model
modelData <- select(wifiData, c(WAP001:WAP520, BUILDINGID))
test <- run_knn(modelData, "BUILDINGID")
postResample(test$Predictions, test$BUILDINGID)

# Accuracy     Kappa 
# 0.9558233 0.9293653 

#FLOOR model
modelData <- select(wifiData, c(WAP001:WAP520, FLOOR))
test <- run_knn(modelData, "FLOOR")
postResample(test$Predictions, test$FLOOR)
 
# Accuracy     Kappa 
# 0.7692308 0.7026485 

#LAT model
modelData <- select(wifiData, c(WAP001:WAP520, LATITUDE))
test <- run_knn(modelData, "LATITUDE")
postResample(test$Predictions, test$LATITUDE)

# RMSE   Rsquared        MAE 
# 12.6229449  0.9601962  7.9544519 

#LNG model
modelData <- select(wifiData, c(WAP001:WAP520, LONGITUDE))
test <- run_knn(modelData, "LONGITUDE")
postResample(test$Predictions, test$LONGITUDE)

# RMSE   Rsquared        MAE 
# 20.5744473  0.9738948 11.3470134 

