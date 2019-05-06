
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

# Replace variables with RSSI >90, <30 with -200 -----------------------------

wifiData <- bind_cols(wifiData %>% select(contains("WAP")) %>% replace(. > -10 | . < -90, -200),
                      wifiData %>% select(-contains("WAP")))

wifiVerification <- bind_cols(wifiVerification %>% select(contains("WAP")) %>% replace(. > -10 | . < -90, -200),
                              wifiVerification %>% select(-contains("WAP")))

# Remove variables with variance 0 ----------------------------------------
constantVars <- which(apply(wifiData, 2, var) == 0)

if(length(constantVars) > 0){
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
model = "svmLinear"

# FLOOR model -------------------------------------------------------------
dependant = "FLOOR"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, model, dependant)

printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")

# Verification data
wifiVerification$PredictionsFLOOR <- predict(results$model, select(wifiVerification, c(contains("WAP"))))

printresult <- postResample(wifiVerification$PredictionsFLOOR, pull(wifiVerification[,c(dependant)]))
cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")

# LAT model ---------------------------------------------------------------
dependant = "LATITUDE"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, model, dependant)

printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")

# Verification data
wifiVerification$PredictionsLAT <- predict(results$model, select(wifiVerification, c(contains("WAP"))))

printresult <- postResample(wifiVerification$PredictionsLAT, pull(wifiVerification[,c(dependant)]))
cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")

coordinates <- data.frame(LATpred = wifiVerification$PredictionsLAT, LATact = pull(wifiVerification[,c(dependant)]))

# LNG model ---------------------------------------------------------------
dependant = "LONGITUDE"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, model, dependant)

printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")

# Verification data
wifiVerification$PredictionsLNG <- predict(results$model, select(wifiVerification, c(contains("WAP"))))

printresult <- postResample(wifiVerification$PredictionsLNG, pull(wifiVerification[,c(dependant)]))
cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")

coordinates <- cbind(coordinates, LNGpred = wifiVerification$PredictionsLNG, LNGact = pull(wifiVerification[,c(dependant)]))

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

#Check erros on the map
cord.EPSG3857 <- SpatialPoints(cbind(checkexceptions$PredictionsLNG, checkexceptions$PredictionsLAT), proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
cord.EPSG4326 <- spTransform(cord.EPSG3857, CRS = "+proj=longlat +datum=WGS84 +no_defs")
checkexceptions$mapPredictionsLAT <- cord.EPSG4326@coords[,2]
checkexceptions$mapPredictionsLNG <- cord.EPSG4326@coords[,1]
