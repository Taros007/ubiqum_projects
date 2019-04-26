## Load libraries ================================
library(tidyverse)
library(plotly)
library(ggfortify)
library(forecast)

## Load preprocessed data ========================
powerData <- readRDS('./output/powerData.RDS')

# #Exploration
# plot(powerData$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(powerData, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

## Create 1-minute frequency plot for one day ==========

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(powerData, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Create 10-minute frequency plot for one day ==========

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(powerData, 
                     year == 2008 & 
                       month == 1 &
                       day == 9 & 
                       (minute == 0 | 
                          minute == 10 | 
                          minute == 20 | 
                          minute == 30 | 
                          minute == 40 | 
                          minute == 50
                        )
                     )

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, 
        x = ~houseDay10$DateTime, 
        y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines'
        ) %>%
  add_trace(y = ~houseDay10$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines'
            ) %>%
  add_trace(y = ~houseDay10$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines'
            ) %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)")
         )

## Week plot sub-meter 1, 2 and 3 with =================

week10 <- filter(powerData, 
                     year == 2008 & 
                       week == 20 & 
                       minute == 0
)

plot_ly(week10, 
        x = ~week10$DateTime, 
        y = ~week10$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines'
) %>%
  add_trace(y = ~week10$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines'
  ) %>%
  add_trace(y = ~week10$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines'
  ) %>%
  layout(title = "Power Consumption week 20, 2008",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (watt-hours)")
  )

## Daily use at noon on Sundays in 2008 - plot sub-meter 1, 2 and 3 =================

week10 <- filter(powerData, 
                 year == 2008 & 
                 weekday == "Sunday" & 
                 hour == 12 &
                 minute == 0
                 )

plot_ly(week10, 
        x = ~week10$DateTime, 
        y = ~week10$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines'
) %>%
  add_trace(y = ~week10$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines'
  ) %>%
  add_trace(y = ~week10$Sub_metering_3, 
            name = 'Water Heater & AC', 
            mode = 'lines'
  ) %>%
  layout(title = "Power Consumption on Sundays in 2008 at noon",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)")
  )

## Plot one subset of data

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(powerData, year %in% c(2007:2009) & weekday == "Monday" & hour == 20 & minute == 0)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_070809weekly)

ggseasonplot(tsSM3_070809weekly, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")


## Modelling ========================

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

# Model Sub-meter 1

subm1data <- filter(powerData, year %in% c(2008) & weekday == "Sunday")

## Create TS object with SubMeter3
tsSM1_08sunday <- ts(subm1data$Sub_metering_1, frequency=1440, start=c(2008,1))

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM1 <- tslm(tsSM1_08sunday ~ trend + season)
summary(fitSM1)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods
forecastfitSM1 <- forecast(fitSM1, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")


## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3
summary(components070809SM3weekly)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))
