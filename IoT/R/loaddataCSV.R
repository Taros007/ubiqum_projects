## Load libraries ======================
library(tidyverse)
library(datetime)
library(lubridate)

## Load data ===========================
powerData <- read_delim('./input/household_power_consumption.txt', delim = ";", na = "?")

## Combine Date and Time attribute values in a new attribute column
powerData$DateTime <- paste(powerData$Date,powerData$Time)
powerData$DateTime <- as.POSIXct(powerData$DateTime, "%d/%m/%Y %H:%M:%S", tz = "Europe/Paris")

## Create "year" attribute with lubridate
powerData$year <- year(powerData$DateTime)
powerData$month <- month(powerData$DateTime)
powerData$quarter <- quarter(powerData$DateTime)
powerData$day <- day(powerData$DateTime)
powerData$hour <- hour(powerData$DateTime)
powerData$minute <- minute(powerData$DateTime)

## Calculate power used by remainder of house ===============
#from data notes: 1.(global_active_power*1000/60 - sub_metering_1 
#- sub_metering_2 - sub_metering_3) 
#represents the active energy consumed every minute (in watt hour) in the household 
#by electrical equipment not measured in sub-meterings 1, 2 and 3. 
#Negative values are change to 0, probably result from missing digits in Global_active_power

powerData$Sub_unnumbered <- sapply((powerData$Global_active_power * 1000 / 60 - 
  powerData$Sub_metering_1 - 
  powerData$Sub_metering_2 - 
  powerData$Sub_metering_3), 
  function(x) max(x,0)
  ) 
  
#Reorder colums, and leave out original Date and Time fields
powerData <- powerData[c(3:9,17,10:16)]

#Save end-result for further processing
saveRDS(powerData, './output/powerData.RDS')
