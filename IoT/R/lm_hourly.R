## Load libraries ================================
library(tidyverse)
library(magrittr)
library(lubridate)
library(caret)
library(doParallel)

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes.
cl <- makeCluster(3)

# Register Cluster
registerDoParallel(cl)

## Load preprocessed data ========================
powerData <- readRDS('./output/powerData.RDS')

## Calculate average temperature per day ===============================
# Load weather data
weather <- readRDS('./input/weatherinfo.RDS')
names(weather)[names(weather) == 'Date'] <- 'date'

powerData$date <- as_date(paste(powerData$year, powerData$month, powerData$day, sep="-"), "%Y-%m-%d")

weather %<>% gather(key = hour, value = avg_temp, hour0, hour1, hour2, hour3, hour4, hour5, hour6, hour7,  hour8, hour9, hour10, hour11, hour12, hour13, hour14, hour15, hour16, hour17, hour18, hour19, hour20, hour21, hour22, hour23)

weather$hour[weather$hour== "hour0"] <- 0
weather$hour[weather$hour== "hour1"] <- 1
weather$hour[weather$hour== "hour2"] <- 2
weather$hour[weather$hour== "hour3"] <- 3
weather$hour[weather$hour== "hour4"] <- 4
weather$hour[weather$hour== "hour5"] <- 5
weather$hour[weather$hour== "hour6"] <- 6
weather$hour[weather$hour== "hour7"] <- 7
weather$hour[weather$hour== "hour8"] <- 8
weather$hour[weather$hour== "hour9"] <- 9
weather$hour[weather$hour== "hour10"] <- 10
weather$hour[weather$hour== "hour11"] <- 11
weather$hour[weather$hour== "hour12"] <- 12
weather$hour[weather$hour== "hour13"] <- 13
weather$hour[weather$hour== "hour14"] <- 14
weather$hour[weather$hour== "hour15"] <- 15
weather$hour[weather$hour== "hour16"] <- 16
weather$hour[weather$hour== "hour17"] <- 17
weather$hour[weather$hour== "hour18"] <- 18
weather$hour[weather$hour== "hour19"] <- 19
weather$hour[weather$hour== "hour20"] <- 20
weather$hour[weather$hour== "hour21"] <- 21
weather$hour[weather$hour== "hour22"] <- 22
weather$hour[weather$hour== "hour23"] <- 23
weather$hour <- as.numeric(weather$hour)

model_powerData <- powerData %>%
  filter(year == 2009) %>% #temporary filter to speed up testing
  group_by(date, year, weekday, month, day, hour) %>% 
  summarize(total_energy_use = sum(Sub_metering_1)) %>% 
  ungroup()

model_powerData <- left_join(model_powerData, weather, by = c("date", "hour"))
  
model_powerData %<>% 
  select(weekday, total_energy_use, hour, month, avg_temp)#total_energy_use, avg_temp, weekend, hour)

model_powerData %<>% mutate(
  month = as.factor(month),
  hour = as.factor(hour),
  weekday = as.factor(weekday)
  )

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = model_powerData, fullRank = T)
model_powerData <- data.frame(predict(newDataFrame, newdata = model_powerData))

train_id <- createDataPartition(y = model_powerData$total_energy_use, p = 0.8, list = F)
train <- model_powerData[train_id,]
test <- model_powerData[-train_id,]

rfFit1 <- caret::train(total_energy_use~. ,
                       data = train,
                       method = "brnn"
                       #xpreProcess = c("center", "scale")
)

test$Predictions <- predict(rfFit1, test)
postResample(test$Predictions, test$total_energy_use)

#Check important variables
varTun <- varImp(rfFit1)
plot(varTun, main = "Top variance importance")

ggplot(test, aes(x = total_energy_use, y = Predictions)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

