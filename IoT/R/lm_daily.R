## Load libraries ================================
library(tidyverse)
library(magrittr)
library(lubridate)
library(caret)
library(doParallel)
source('./R/BBC_style.R')

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

weather$avg_temp <- rowMeans(select(weather,hour0:hour23))
powerData$avg_temp <- as.numeric(weather$avg_temp[match(date(powerData$DateTime), weather$date)])

powerData$date <- as_date(paste(powerData$year, powerData$month, powerData$day, sep="-"), "%Y-%m-%d")

model_powerData <- powerData %>%
  #filter(year == 2009) %>% #temporary filter to speed up testing
  group_by(date, avg_temp, year, weekday, week, month, day) %>% 
  summarize(total_energy_use = sum(total_energy_use)) %>% 
  ungroup()
  
model_powerData %<>% 
  select(date, year, weekday, total_energy_use, week)#total_energy_use, avg_temp, weekend, hour)

model_powerData$lag1 <- c(0, model_powerData$total_energy_use[seq_along(model_powerData$total_energy_use) -1])
model_powerData$lagweek <- c(0,0,0,0,0,0,0, model_powerData$total_energy_use[seq_along(model_powerData$total_energy_use) -7])

model_powerData %<>% mutate(
  #month = as.factor(month),
  week = as.factor(week),
  weekday = as.factor(weekday)
  #weekend = as.factor(weekend)
  )

## Dummify data =================================
newDataFrame <- dummyVars(" ~ .", data = model_powerData, fullRank = T)
model_powerData <- data.frame(predict(newDataFrame, newdata = model_powerData))

train <- model_powerData %>% filter(year %in% c(2006, 2007, 2008, 2009))
test <- model_powerData %>% filter(year == 2010)

# train_id <- createDataPartition(y = model_powerData$total_energy_use, p = 0.8, list = F)
# train <- model_powerData[train_id,]
# test <- model_powerData[-train_id,]

rfFit1 <- caret::train(total_energy_use~. ,
                       data = train %>% select(-date, -year),
                       method = "lm",
                       preProcess = c("scale")
)

test$Predictions <- predict(rfFit1, test)
postResample(test$Predictions, test$total_energy_use)

test %>% 
  ggplot(aes(x = as_date(date))) +
  geom_line(aes(y = total_energy_use), color = "#1380A1") +
  geom_line(aes(y = Predictions), color = "#990000") +
  geom_line(data = train %>% filter(year == 2009), aes(y = total_energy_use), color = "#FAAB18") +
  scale_colour_manual(values = c("#1380A1", "#990000", "#FAAB18","#588300")) +
  bbc_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Total energy use",
       subtitle = "Predictions LM")

#Check important variables
varTun <- varImp(rfFit1)
plot(varTun, main = "Top variance importance")

ggplot(test, aes(x = total_energy_use, y = Predictions)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

