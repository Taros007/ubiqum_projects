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
powerData$weekday <- weekdays(powerData$DateTime)
powerData$weekend <- sapply(powerData, function(x) ifelse(powerData$weekday %in% c("Saturday", "Sunday"), 1, 0))

powerData$weekday <- factor(powerData$weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

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

powerData$total_energy_use <- sapply((powerData$Global_active_power * 1000 / 60), 
                                   function(x) max(x,0)
) 
  
#Reorder colums, and leave out original Date and Time fields
powerData <- powerData[c(3:9,19,20,10:18)]

#Explore NAs
# sapply(powerData, function(x) sum(is.na(x)))
# 
# y <- powerData[!complete.cases(powerData), ]
# #Which days have NAs
# unique(as.Date(y$DateTime))
# #How many NAs per day?
# group_by(y, date(DateTime)) %>%
#   summarize(n = n()) %>%
#   arrange(desc(n)) %>%
#   print(n = nrow(.))

#Fix NAs by taking last weeks' value (comparable day) ==================
# Identify index of all NAs, and substract 10080 (minutes in a week) from it if possible
# Subsequently, replace all NAs
y <- ifelse(which(is.na(powerData[1])) >= 10080, 
            which(is.na(powerData[1]))-10080, 
            which(is.na(powerData[1])))
z <- which(is.na(powerData[1]))
powerData[z,c(1:8)] <- powerData[y,c(1:8)]
remove(y,z)

# Omit NAs that couldn't be imputed (first week of dataset)
powerData %<>% na.omit()

#Save end-result for further processing
saveRDS(powerData, './output/powerData.RDS')

