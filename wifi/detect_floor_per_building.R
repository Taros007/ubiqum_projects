
# Script control ----------------------------------------------------------
replace_RSSI_strongweak <- T
test_juan <- T
normal_scaling <- F

# Load data ---------------------------------------------------------------
source('./R/data_loading.R')
source('./R/data_loading_verification.R')

wifiData %<>% mutate(
  FLOOR = as.factor(FLOOR),
  BUILDINGID = as.factor(BUILDINGID),
  SPACEID = as.factor(SPACEID),
  RRELATIVEPOSITION = as.factor(RELATIVEPOSITION),
  USERID = as.factor(USERID),
  PHONEID = as.factor(PHONEID),
  TIMESTAMP = as_datetime(TIMESTAMP, tz = "Europe/Madrid")
)

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(DMwR)

# Replace variables with RSSI >90, <30 with -200 -----------------------------
if (replace_RSSI_strongweak) {
  wifiData <- bind_cols(wifiData %>% select(contains("WAP")) %>% replace(. > -10 | . < -90, -200),
                        wifiData %>% select(-contains("WAP")))
  
  wifiVerification <- bind_cols(wifiVerification %>% select(contains("WAP")) %>% replace(. > -10 | . < -90, -200),
                                wifiVerification %>% select(-contains("WAP")))
}


# Normalization -----------------------------------------------------------
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

# Building 2 --------------------------------------------------------------
wifiData_BUILDING2 <- wifiData %>% 
  filter(BUILDINGID == 1) %>% 
  select(-TIMESTAMP)

wifiVerification_BUILDING2 <- wifiVerification %>% 
  filter(BUILDINGID == 1) %>% 
  select(-TIMESTAMP)

wifiData_BUILDING2_independ <- wifiData_BUILDING2 %>% select(contains("WAP"))
wifiData_BUILDING2_depend <- wifiData_BUILDING2 %>% select(-contains("WAP"))

#create specific dataframe for floors of building 1
B1_floor1_train <- wifiData_BUILDING2_independ %>% filter(FLOOR == 1 &
                                                    LONGITUDE > -7530 &
                                                    LONGITUDE < -7450 &
                                                    LATITUDE > 4864835 &
                                                    LATITUDE < 4864905)


B1_floor0_train <- wifiData_BUILDING2_independ %>% filter(FLOOR == 0 &
                                                    LATITUDE < 4864905 & 
                                                    LATITUDE > 4864875 &
                                                    LONGITUDE > -7510 & 
                                                    LONGITUDE < -7470)

B1_floor_rest_train <- wifiData_BUILDING2_independ %>% filter(!((FLOOR == 1 &
                                                           LONGITUDE > -7530 &
                                                           LONGITUDE < -7450 &
                                                           LATITUDE > 4864835 &
                                                           LATITUDE < 4864905) |
                                                          
                                                          FLOOR == 0 & 
                                                          LATITUDE < 4864905 & 
                                                          LATITUDE > 4864875 &
                                                          LONGITUDE > -7510 & 
                                                          LONGITUDE < -7470))


#change WAP values of specific part of floor 1
B1_floor1_train[B1_floor1_train < -72] <- 100
B1_floor1_train[B1_floor1_train < -70] <- 100

#bind rows back together
wifiData_BUILDING2_independ <- bind_rows(B1_floor1_train, 
                                 B1_floor_rest_train, 
                                 B1_floor0_train)

wifiData_BUILDING2 <- bind_cols(wifiData_BUILDING2_independ, wifiData_BUILDING2_depend)

# Sampling ----------------------------------------------------------------
table(wifiData_BUILDING2$FLOOR)
wifiData_BUILDING2$FLOOR %<>% droplevels
#BUILDINGID 2 upsampled_wifiData_BUILDING2 <- as_tibble(SMOTE(FLOOR ~ ., as.data.frame(wifiData_BUILDING2), perc.over = 100, perc.under = 800))
#Buildingid 1
upsampled_wifiData_BUILDING2 <- as_tibble(SMOTE(FLOOR ~ ., as.data.frame(wifiData_BUILDING2), perc.over = 100, perc.under = 600))
table(upsampled_wifiData_BUILDING2$FLOOR)

#Create training and testing sets
set.seed(541)
train_ids <- sample(seq_len(nrow(upsampled_wifiData_BUILDING2)), size = floor(0.75 * nrow(upsampled_wifiData_BUILDING2)))

train <- upsampled_wifiData_BUILDING2[train_ids,]
test <- upsampled_wifiData_BUILDING2[-train_ids,]

source('./R/run_model.R')
model <- "knn"

# FLOOR model without BUILDINGID -------------------------------------------------------------
dependant = "FLOOR"
trainData <- select(train, c(contains("WAP"), dependant))
testData <- select(test, c(contains("WAP"), dependant))
results <- run_model(trainData, testData, model, dependant)

printresult <- postResample(results$predictions, pull(testData[,c(dependant)]))
cat("Resampling results for training of label", dependant, "with model", model, ":\n", printresult, "\n")

# Verification data
wifiVerification_BUILDING2$PredictionsFLOOR <- predict(results$model, select(wifiVerification_BUILDING2, c(contains("WAP"))))
printresult <- postResample(wifiVerification_BUILDING2$PredictionsFLOOR, pull(wifiVerification_BUILDING2[,c(dependant)]))
cat("Resampling results for verification of label", dependant, "with model", model, ":\n", printresult, "\n")

