## Load libraries ================================
library(tidyverse)
library(lubridate)
library(forecast)

## Load preprocessed data ========================
powerData <- readRDS('./output/powerData.RDS')

## Create data per week per submeter ==========================
powerWeek <- powerData %>% 
  group_by(year, month, day, week, weekday, day) %>% 
  summarise('meter1' = mean(Sub_metering_1),
            'meter2' = mean(Sub_metering_2), 
            'meter3' = mean(Sub_metering_3),
            'meternon' = mean(Sub_unnumbered)) %>% 
  mutate(date = as_date(paste(year, month, day, sep="-"), "%Y-%m-%d"),
         totalpower = meter1 + meter2 + meter3 + meternon) 

ggplot(powerWeek, aes(x = date, y = meter1)) +
  geom_line()

powerWeekTS <- powerData %>% 
  filter(year == 2008, week %in% c(2,3,4)) %>% 
  mutate(date = as_date(paste(year, month, day, sep="-"), "%Y-%m-%d"),
         totalpower = Global_active_power * 1000 / 60) 

powerWeekTS <- ts(powerWeek$totalpower, frequency = 365, 
                  start = c(powerWeek$year[1], powerWeek$day[1]))

library(doParallel)

## Prepare clusters =================================
cl <- makeCluster(3)
registerDoParallel(cl)

taylor <- msts(powerWeek$totalpower, seasonal.periods=c(60, 1440, 10080))
taylor.fit <- tbats(taylor)
plot(forecast(taylor.fit, h = 1440))


test <- HoltWinters(powerWeekTS)                  
plot(test, ylim = c(0, 25))
forecasttest <- forecast(test, h = 90)
plot(forecasttest)
# 
# y <- head(powerWeek, 1000)
# View(y)

decomp <- stl(powerWeekTS, s.window="periodic")
ap.sa <- seasadj(decomp)
autoplot(cbind(powerWeekTS, SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Number of passengers (thousands)")

ap.sa %>% diff() %>% ggtsdisplay(main="")
fit <- auto.arima(ap.sa)
checkresiduals(fit)
autoplot(forecast(fit))
