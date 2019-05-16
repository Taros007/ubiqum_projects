
# Script control ----------------------------------------------------------
remove_0_var <- T
replace_RSSI_strongweak <- T
remove_userID6_weird_signals <- T
scale_deviceID <- F
test_juan <- T
normal_scaling <- F
model = "knn"

# Load data ---------------------------------------------------------------
source('./R/data_loading.R')
source('./R/data_loading_verification.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)

# Select building 0 observations ------------------------------------------
# wifiData %<>% filter(wifiData$BUILDINGID == 1)
# wifiVerification %<>% filter(wifiVerification$BUILDINGID == 1)

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
if (replace_RSSI_strongweak) {
  wifiData <- bind_cols(wifiData %>% select(contains("WAP")) %>% replace(. > -30 | . < -90, -200),
                        wifiData %>% select(-contains("WAP")))
  
  wifiVerification <- bind_cols(wifiVerification %>% select(contains("WAP")) %>% replace(. > -10 | . < -90, -200),
                                wifiVerification %>% select(-contains("WAP")))
  }

# Remove high value observation for user 6 --------------------------------
if (remove_userID6_weird_signals) {
  wifiData[wifiData$USERID==6,] <- bind_cols(wifiData %>% filter(USERID == 6) %>% select(contains("WAP")) %>% replace(. > -25, -200),
                                             wifiData %>% filter(USERID == 6) %>% select(-contains("WAP")))
}

# Remove variables with variance 0 ----------------------------------------
if (remove_0_var) {
  constantVars <- which(apply(wifiData, 2, var) == 0)
  
  if(length(constantVars) > 0){
    wifiData <- wifiData[,-constantVars]
    wifiVerification <- wifiVerification[,-constantVars]
  }
}

# Scale each observation to account for device differences ----------------
if (scale_deviceID) {
  wifiData <- 
    bind_cols(wifiData %>% 
      select(contains("WAP")) %>% 
      replace(. == -200, NA) %>%
      mutate_all(funs((. - min(., na.rm = T))/(max(., na.rm = T) - min(., na.rm = T)))) %>% 
      as_tibble(),
      wifiData %>% select(-contains("WAP")))
  wifiData[is.na(wifiData)] <- 0

  wifiVerification <- bind_cols(wifiVerification %>% 
                                  select(contains("WAP")) %>% 
                                  replace(. == -200, NA) %>%
                                  mutate_all(funs((. - min(., na.rm = T))/(max(., na.rm = T) - min(., na.rm = T)))) %>% 
                                  as_tibble(),
                                  wifiVerification %>% select(-contains("WAP")))
  wifiVerification[is.na(wifiVerification)] <- 0
}

if (test_juan) {
  temp <- as.matrix(wifiData %>% select(starts_with("WAP")))
  
  output_min <- vector(mode = "numeric", length = nrow(temp))
  output_max <- vector(mode = "numeric", length = nrow(temp))
  output_row <- vector(mode = "numeric", length = ncol(temp))
  for (i in 1:nrow(temp)) {
    
    # vector of values by row
    for (j in 1:ncol(temp)) {
      if (temp[[i,j]] != -200) {
        output_row[j] <- temp[[i,j]]
      } else {
        output_row[j] <- NA
      }
    }
    
    # Minumum values by row
    if (abs(min(output_row, na.rm = TRUE)) != Inf) {
      output_min[i] <- min(output_row, na.rm = TRUE)
    } else {output_min[i] <- NA}
    
    # Maximum values by row
    if (abs(min(output_row, na.rm = TRUE)) != Inf) {
      output_max[i] <- max(output_row, na.rm = TRUE)
    } else {output_max[i] <- NA}
  }
  
  # normalize the
  temp_norm <- temp
  for (i in 1:nrow(temp)) {
    for (j in 1:ncol(temp)) {
      x <- ((temp[[i,j]] - output_min[i]) /
              (output_max[i] - output_min[i]))
      if (is.na(x) | x < 0) {
        temp_norm[[i,j]] <- 0
      } else {
        temp_norm[[i,j]] <- x
      }
    }
  }
  temp_norm <- as_tibble(temp_norm)
  wifiData <- bind_cols(temp_norm, wifiData %>% select(-contains("WAP")))
  
  temp <- as.matrix(wifiVerification %>% select(starts_with("WAP")))
  
  output_min <- vector(mode = "numeric", length = nrow(temp))
  output_max <- vector(mode = "numeric", length = nrow(temp))
  output_row <- vector(mode = "numeric", length = ncol(temp))
  for (i in 1:nrow(temp)) {
    
    # vector of values by row
    for (j in 1:ncol(temp)) {
      if (temp[[i,j]] != -200) {
        output_row[j] <- temp[[i,j]]
      } else {
        output_row[j] <- NA
      }
    }
    
    # Minumum values by row
    if (abs(min(output_row, na.rm = TRUE)) != Inf) {
      output_min[i] <- min(output_row, na.rm = TRUE)
    } else {output_min[i] <- NA}
    
    # Maximum values by row
    if (abs(min(output_row, na.rm = TRUE)) != Inf) {
      output_max[i] <- max(output_row, na.rm = TRUE)
    } else {output_max[i] <- NA}
  }
  
  # normalize the
  temp_norm <- temp
  for (i in 1:nrow(temp)) {
    for (j in 1:ncol(temp)) {
      x <- ((temp[[i,j]] - output_min[i]) /
              (output_max[i] - output_min[i]))
      if (is.na(x) | x < 0) {
        temp_norm[[i,j]] <- 0
      } else {
        temp_norm[[i,j]] <- x
      }
    }
  }
  temp_norm <- as_tibble(temp_norm)
  wifiVerification <- bind_cols(temp_norm, wifiVerification %>% select(-contains("WAP")))
}

if (normal_scaling) {
  wifiData <- as_tibble(cbind(t(scale(t(wifiData %>% select(contains("WAP"))))),
                    wifiData %>% select(-contains("WAP")))) %>% na.omit()
  wifiVerification <- as_tibble(cbind(t(scale(t(wifiVerification %>% select(contains("WAP"))))),
                                      wifiVerification %>% select(-contains("WAP"))) %>% na.omit()
  )
}

# Modelling ---------------------------------------------------------------
#Create training and testing sets
set.seed(541)
train_ids <- sample(seq_len(nrow(wifiData)), size = floor(0.75 * nrow(wifiData)))

train <- wifiData[train_ids,]
test <- wifiData[-train_ids,]

source('./R/run_model.R')

# BUILDINGID model -------------------------------------------------------------
dependant = "BUILDINGID"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, model, dependant)

printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")

train$PredictionsBUILDINGID <- predict(results$model, train)
test$PredictionsBUILDINGID <- predict(results$model, test)

# Verification data
wifiVerification$PredictionsBUILDINGID <- predict(results$model, select(wifiVerification, c(contains("WAP"))))
printresult <- postResample(wifiVerification$PredictionsBUILDINGID, pull(wifiVerification[,c(dependant)]))
cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")

# FLOOR model without BUILDINGID -------------------------------------------------------------
# dependant = "FLOOR"
# trainData <- select(train, c(contains("WAP"), dependant))
# testData <- select(test, c(contains("WAP"), dependant))
# results <- run_model(trainData, testData, model, dependant)
# 
# printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
# cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")
# 
# # Verification data
# wifiVerification$PredictionsFLOOR <- predict(results$model, select(wifiVerification, c(contains("WAP"))))
# printresult <- postResample(wifiVerification$PredictionsFLOOR, pull(wifiVerification[,c(dependant)]))
# cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")

# FLOOR model with BUILDINGID -------------------------------------------------------------
dependant = "FLOOR"
trainData <- select(train, c(contains("WAP"), dependant, PredictionsBUILDINGID))
testData <- select(test, c(contains("WAP"), dependant, PredictionsBUILDINGID))
results <- run_model(trainData, testData, model, dependant)

printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")

train$PredictionsFLOOR <- predict(results$model, train)
test$PredictionsFLOOR <- predict(results$model, test)

# Verification data
wifiVerification$PredictionsFLOOR <- predict(results$model, select(wifiVerification, c(contains("WAP"), PredictionsBUILDINGID)))
printresult <- postResample(wifiVerification$PredictionsFLOOR, pull(wifiVerification[,c(dependant)]))
cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")

# LAT model ---------------------------------------------------------------
# dependant = "LATITUDE"
# trainData <- select(train, c(contains("WAP"), dependant))
# testData <- select(test, c(contains("WAP"), dependant))
# results <- run_model(trainData, testData, model, dependant)
# 
# printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
# cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")
# 
# # Verification data
# wifiVerification$PredictionsLAT <- predict(results$model, select(wifiVerification, c(contains("WAP"))))
# printresult <- postResample(wifiVerification$PredictionsLAT, pull(wifiVerification[,c(dependant)]))
# cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")
# 
# coordinates <- data.frame(LATpred = wifiVerification$PredictionsLAT, LATact = pull(wifiVerification[,c(dependant)]))

# LNG model ---------------------------------------------------------------
# dependant = "LONGITUDE"
# trainData <- select(train, c(contains("WAP"), dependant))
# testData <- select(test, c(contains("WAP"), dependant))
# results <- run_model(trainData, testData, model, dependant)
# 
# printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
# cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")
# 
# # Verification data
# wifiVerification$PredictionsLNG <- predict(results$model, select(wifiVerification, c(contains("WAP"))))
# printresult <- postResample(wifiVerification$PredictionsLNG, pull(wifiVerification[,c(dependant)]))
# cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")
# 
# coordinates <- cbind(coordinates, LNGpred = wifiVerification$PredictionsLNG, LNGact = pull(wifiVerification[,c(dependant)]))

# LAT model with BUILDINGID and FLOOR ---------------------------------------------------------------
dependant = "LATITUDE"
trainData <- select(train, c(contains("WAP"), dependant, PredictionsBUILDINGID, PredictionsFLOOR))
testData <- select(test, c(contains("WAP"), dependant, PredictionsBUILDINGID, PredictionsFLOOR))
results <- run_model(trainData, testData, model, dependant)

printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")

# Verification data
wifiVerification$PredictionsLAT <- predict(results$model, select(wifiVerification, c(contains("WAP"), PredictionsBUILDINGID, PredictionsFLOOR)))
printresult <- postResample(wifiVerification$PredictionsLAT, pull(wifiVerification[,c(dependant)]))
cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")

coordinates <- data.frame(LATpred = wifiVerification$PredictionsLAT, LATact = pull(wifiVerification[,c(dependant)]))

# LNG model with BUILDINGID and FLOOR ---------------------------------------------------------------
dependant = "LONGITUDE"
trainData <- select(train, c(contains("WAP"), dependant, PredictionsBUILDINGID, PredictionsFLOOR))
testData <- select(test, c(contains("WAP"), dependant, PredictionsBUILDINGID, PredictionsFLOOR))
results <- run_model(trainData, testData, model, dependant)

printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")

# Verification data
wifiVerification$PredictionsLNG <- predict(results$model, select(wifiVerification, c(contains("WAP"), PredictionsBUILDINGID, PredictionsFLOOR)))
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
  geom_density() +
  labs(title = "Position prediction error", 
       subtitle = "for verification data, in m") +
  bbc_style()

#View sum of total RSSI-level
wifiVerification$sum <- rowSums(wifiVerification[, colnames(wifiVerification %>% select(contains("WAP")))])
checkexceptions <- wifiVerification %>% filter(distance > 50)

#Check errors on the map
cord.EPSG3857 <- SpatialPoints(cbind(checkexceptions$PredictionsLNG, checkexceptions$PredictionsLAT), proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
cord.EPSG4326 <- spTransform(cord.EPSG3857, CRS = "+proj=longlat +datum=WGS84 +no_defs")
checkexceptions$mapPredictionsLAT <- cord.EPSG4326@coords[,2]
checkexceptions$mapPredictionsLNG <- cord.EPSG4326@coords[,1]
