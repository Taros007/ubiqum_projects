# Load libraries ----------------------------------------------------------
library(tidyverse)
library(plotly)
library(ggfortify)
library(lubridate)
library(scales)
library(doParallel)
library(magrittr)
source('./R/BBC_style.R')

# Load clusters -----------------------------------------------------------
cl <- makeCluster(3)
registerDoParallel(cl)

# Load preprocessed data --------------------------------------------------
powerData <- readRDS('./output/powerData.RDS')

## Subset data
sample <- powerData %>% 
  select(DateTime, year, month, day, hour, minute, Sub_metering_1) %>% 
  #filter(year == 2007 & month %in% c(1,8) & day %in% seq(1,31)) %>% 
  rename(data = Sub_metering_1) %>% 
  mutate(data = data * 60 / 1000)

plot_ly(sample, x = ~DateTime, y = ~data, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption analysis",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))


## detect dishwasher

#tracker tracks time of energy use
#peak tracks how many peaks

sample$lag1 <- lag(sample$data, 1)
sample$lag2 <- lag(sample$data, 2)
tracker <- 0
peak <- 0
avg_energy <- 0
occur0 <- 0
found <- 0
dishwasher_found <- setNames(
  data.frame(stringsAsFactors=FALSE,
             matrix(
               ncol = 3, nrow = 0)), 
            c("Date",
              "Washing_found",
              "avg_energy"
              )
            )

for (i in 5:nrow(sample)) {
  if (tracker > 0) {
    avg_energy <- (avg_energy * tracker + sample$data[i]) / tracker
    tracker <- tracker + 1
    if (sample$lag1[i] - sample$data[i] < -0.5) {
      peak <- peak + 1
    }
    if (tracker > 90 & peak == 2) {
      cat("Dishwasher found at:", as.character(as_date(sample$DateTime[i]), tz = "Europe/Paris"), ".", "Average energy use per minute:", avg_energy, "\n")
      new_line <- c(Date = sample$DateTime[i], avg_energy = avg_energy)
      #washing_machine <- dplyr::bind_rows(washing_machine, c(sample$Date[i-1], found, avg_energy))
      dishwasher_found <- dplyr::bind_rows(dishwasher_found, new_line)
      #found <- 0                       
      avg_energy <- 0
      occur0 <- 0
      peak <- 0
      tracker <- 0
      #found <- 1
    }
    if (sample$data[i] == 0){
      if (occur0 <= 3){
        occur0 <- occur0 + 1
      }
      else {
        occur0 <- 0
        tracker <- 0
        avg_energy <- 0
        peak <- 0
      }
    }
  }
  else if (sample$lag1[i] == 0 & sample$data[i] != 0) {
    tracker <- 1
  }
  # if (tracker > 1) {
  #   cat(as.character(as_Date(sample$Date[i]), tz = "Europe/Paris"), "i=", i, "tracker=", tracker, "peak=", peak, "occur0=", occur0, "\n")
  # }
}

dishwasher_found$avg_energy <- as.double(dishwasher_found$avg_energy)
saveRDS(dishwasher_found, './output/dishwasher.RDS')

# Stop clusters -----------------------------------------------------------

stopCluster(cl)

# Analysis plot -----------------------------------------------------------

dishwasher <- readRDS('./output/dishwasher.RDS')

dishwasher$year <- year(dishwasher$Date)
dishwasher$month <- month(dishwasher$Date)
dishwasher$quarter <- quarter(dishwasher$Date)
dishwasher$week <- week(dishwasher$Date)
dishwasher$day <- day(dishwasher$Date)
dishwasher$hour <- hour(dishwasher$Date)
dishwasher$minute <- minute(dishwasher$Date)
dishwasher$weekday <- weekdays(dishwasher$Date)
dishwasher$weekend <- sapply(dishwasher, function(x) ifelse(dishwasher$weekday %in% c("Saturday", "Sunday"), 1, 0))
dishwasher$weekday <- factor(dishwasher$weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Remove outliers ---------------------------------------------------------

dishwasher <- dishwasher[dishwasher$avg_energy < 5,]

dishwasher %>%
  group_by(year, month) %>% 
  summarize(occurance = length(avg_energy)) %>% 
  ggplot(aes(x = as.Date(paste(year, month, "1", sep = "-"), origin="2012-01-01"), y = occurance)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  scale_x_date(breaks = pretty_breaks(10), labels = date_format("%b-%g")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Use of dishwasher",
       subtitle = "Amount of uses per month")

dishwasher %>%
  group_by(year, month) %>% 
  summarize(avg_energy = mean(avg_energy)) %>% 
  ggplot(aes(x = as.Date(paste(year, month, "1", sep = "-"), origin="2012-01-01"), y = avg_energy)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  scale_x_date(breaks = pretty_breaks(10), labels = date_format("%b-%g")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Use of dishwasher",
       subtitle = "Amount of uses per month")

dishwasher %>%
  group_by(hour) %>% 
  summarize(avg_energy = length(avg_energy)) %>% 
  ggplot(aes(x = hour, y = avg_energy)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Total amount of uses of dishwasher",
       subtitle = "spread over the day")

y <- dishwasher %>% 
  filter(Washing_found == 1)

'''
Oven: 2.3 kWh/hour
Microwave: 0.12 kWh per 5 min, 1.44 kWh/hour
Dishwasher 1.8 kWh/hour

http://www.siliconvalleypower.com/for-residents/save-energy/appliance-energy-use-chart
'''