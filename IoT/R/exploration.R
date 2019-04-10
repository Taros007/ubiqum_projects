## Load libraries ================================
library(tidyverse)
library(magrittr)
library(datetime)
library(lubridate)
library(scales)
source('./R/BBC_style.R')

## Load preprocessed data ========================
powerData <- readRDS('./output/powerData.RDS')

# Check start and end dates for every year =====================
group_by(powerData, year) %>% 
  summarize(min = min(DateTime), max = max(DateTime))

## Plot use over all years ===============================
graphData <- powerData %>% 
  group_by(year, month) %>% 
  summarise(Kitchen = sum(Sub_metering_1),
            'Laundry Room' = sum(Sub_metering_2), 
            'Water heater & A/C' = sum(Sub_metering_3),
            'Non-submetered' = sum(Sub_unnumbered)) %>% 
  gather(Kitchen, 
         'Laundry Room', 
         'Water heater & A/C', 
         'Non-submetered', 
         key = Submeter, 
         value = "Monthly_power")

ggplot(graphData, 
       aes(x = as.Date(paste(year, month, "01", sep="-")), 
           y = Monthly_power / 1e3, 
           colour = Submeter)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300")) +
  bbc_style() +
  scale_x_date(breaks = pretty_breaks(10), labels = date_format("%b-%g")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Monthly energy use",
        subtitle="Measured per submeter (in kWh)")
  # geom_label(aes(x = as.Date(paste(2009, "07", "01", sep="-")), y = 600, label = "I'm quite a long\nannotation over\nthree rows"),
  #            hjust = 0,
  #            vjust = 0.5,
  #            lineheight = 0.8,
  #            colour = "#555555",
  #            fill = "white",
  #            label.size = NA,
  #            family="Helvetica",
  #            size = 6)

## Plot use over 24 hrs ===============================

graphData <- powerData %>% 
  group_by(hour) %>% 
  summarise(Kitchen = mean(Sub_metering_1),
            'Laundry Room' = mean(Sub_metering_2), 
            'Water heater & A/C' = mean(Sub_metering_3),
            'Non-submetered' = mean(Sub_unnumbered)) %>% 
  gather(Kitchen, 
         'Laundry Room', 
         'Water heater & A/C', 
         'Non-submetered', 
         key = Submeter, 
         value = "Hourly_power")

ggplot(graphData, 
       aes(x = hour, 
           y = Hourly_power, 
           colour = Submeter)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300")) +
  bbc_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Average energy use during the day",
       subtitle="Measured per submeter in Wh")
  
## WIP: Plot vs weather ===============================

# Load weather data
weather <- readRDS('./input/weatherinfo.RDS')
names(weather)[names(weather) == 'day'] <- 'date'

powerData$date <- date(powerData$DateTime)

powerData %>% 
  group_by(year) %>% 
  summarize('Total_power_usage' = sum(Global_active_power * 1000 / 60 / 1e3))

# ## Create attribute with lubridate
# # weather$year <- year(weather$date)
# # weather$month <- month(weather$date)
# # weather$quarter <- quarter(weather$date)
# # weather$day <- day(weather$date)

## Plot use per month =============================

graphData <- powerData %>% 
  group_by(month) %>% 
  summarise(Kitchen = mean(Sub_metering_1),
            'Laundry Room' = mean(Sub_metering_2), 
            'Water heater & A/C' = mean(Sub_metering_3),
            'Non-submetered' = mean(Sub_unnumbered)) %>% 
  gather(Kitchen, 
         'Laundry Room', 
         'Water heater & A/C', 
         'Non-submetered', 
         key = Submeter, 
         value = "Monthly_power")

ggplot(graphData, 
       aes(x = as.Date(DateTime,origin="2012-01-01"), 
           y = Monthly_power, 
           colour = Submeter)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300")) +
  bbc_style() +
  scale_x_date(date_labels = "%b",date_breaks = "1 month")
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Average energy use during the year",
       subtitle="Measured per submeter in Wh")


#TODO: plot warm, medium, and cold days energy use
#TODO: calculate total bill for household

