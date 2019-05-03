
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
model = "lm"

# FLOOR model -------------------------------------------------------------
dependant = "FLOOR"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, model, dependant)

cat("Resampling results for training of label", dependant, "with model", model, ":\n")
postResample(results$predictions, pull(testData[,c(dependant)]))

# Without mutate_at
# Accuracy  Kappa 
# 0.7651822 0.6943330

# With mutate_at
# Accuracy  Kappa 
# 0.8461538 0.8000937 

# Verification data
wifiVerification$PredictionsFLOOR <- predict(results$model, select(wifiVerification, c(contains("WAP"))))

cat("Resampling results for verification of label", dependant, "with model", model, ":\n")
postResample(wifiVerification$PredictionsFLOOR, pull(wifiVerification[,c(dependant)]))

# With mutate_at
# Accuracy  Kappa 
# 0.7488749 0.6502643 

# LAT model ---------------------------------------------------------------
dependant = "LATITUDE"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, model, dependant)

cat("Resampling results for training of label", dependant, "with model", model, ":\n")
postResample(results$predictions, pull(testData[,c(dependant)]))

# Without mutate_at
# RMSE        Rsquared   MAE 
# 13.9494770  0.9599392  9.1303813

# With mutate_at
# RMSE        Rsquared   MAE 
# 15.3765811  0.9518108  8.7828804 

# Verification data
wifiVerification$PredictionsLAT <- predict(results$model, select(wifiVerification, c(contains("WAP"))))

cat("Resampling results for verification of label", dependant, "with model", model, ":\n")
postResample(wifiVerification$PredictionsLAT, pull(wifiVerification[,c(dependant)]))

# With mutate_at
# RMSE        Rsquared  MAE 
# 19.5894360  0.9228687 11.0990749 

coordinates <- data.frame(LATpred = wifiVerification$PredictionsLAT, LATact = pull(wifiVerification[,c(dependant)]))

# LNG model ---------------------------------------------------------------
dependant = "LONGITUDE"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, model, dependant)

cat("Resampling results for training of label", dependant, "with model", model, ":\n")
postResample(results$predictions, pull(testData[,c(dependant)]))

# Without mutate_at
# RMSE        Rsquared   MAE 
# 15.2954115  0.9850709  9.6400422 

# With mutate_at
# RMSE        Rsquared   MAE 
# 16.8034956  0.9819268  8.2522013 

# Verification data
wifiVerification$PredictionsLNG <- predict(results$model, select(wifiVerification, c(contains("WAP"))))

cat("Resampling results for verification of label", dependant, "with model", model, ":\n")
postResample(wifiVerification$PredictionsLNG, pull(wifiVerification[,c(dependant)]))

coordinates <- cbind(coordinates, LNGpred = wifiVerification$PredictionsLNG, LNGact = pull(wifiVerification[,c(dependant)]))

# With mutate_at
# RMSE        Rsquared  MAE 
# 34.9828158  0.9183959 15.3340369 

# Calculate distance between actual and pred ------------------------------
coordinates$distance <- sqrt(
  (coordinates$LATpred - coordinates$LATact)^2 + 
  (coordinates$LNGpred - coordinates$LNGact)^2
  )

wifiVerification$distance <- coordinates$distance

coordinates %>% 
  ggplot(aes(x=distance)) + 
  geom_density()

#View sum of total RSSI-level
wifiVerification$sum <- rowSums(wifiVerification[, colnames(wifiVerification %>% select(contains("WAP")))])
checkexceptions <- wifiVerification %>% filter(distance > 50)

cord.EPSG3857 <- SpatialPoints(cbind(checkexceptions$PredictionsLNG, checkexceptions$PredictionsLAT), proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
cord.EPSG4326 <- spTransform(cord.EPSG3857, CRS = "+proj=longlat +datum=WGS84 +no_defs")
checkexceptions$mapPredictionsLAT <- cord.EPSG4326@coords[,2]
checkexceptions$mapPredictionsLNG <- cord.EPSG4326@coords[,1]
