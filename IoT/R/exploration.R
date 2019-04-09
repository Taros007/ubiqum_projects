## Load libraries ================================
library(tidyverse)
library(magrittr)
library(scales)
source('./R/BBC_style.R')

## Load preprocessed data ========================
powerData <- readRDS('./output/powerData.RDS')


summary(powerData)

#Explore NAs
sapply(powerData, function(x) sum(is.na(x)))

y <- powerData[!complete.cases(powerData), ]
#Which days have NAs
unique(as.Date(y$DateTime))
#How many NAs per day?
group_by(y, date(DateTime)) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  print(n = nrow(.))

#Fix NAs by taking last weeks' value (comparable day) ==================

# Identify index of all NAs, and substract 10080 (minutes in a week) from it if possible
# Subsequently, replace all NAs
y <- ifelse(which(is.na(powerData[1])) >= 10080, 
            which(is.na(powerDatay[1]))-10080, 
            which(is.na(powerData[1])))
z <- which(is.na(powerData[1]))
powerData[z,c(1:8)] <- powerData[y,c(1:8)]
remove(y,z)

# Omit NAs that couldn't be imputed (first week of dataset)
powerData %<>% na.omit()

# Check start and end dates for every year =====================
group_by(powerData, year) %>% 
  summarize(min = min(DateTime), max = max(DateTime))

# Initial data exploration via graphs ==========================

powerData <- powerDatacopy

# Get table with monthly energy use per meter
graphData <- powerData %>% 
  group_by(year, month) %>% 
  summarise(Sub_metering_1 = sum(Sub_metering_1),
            Sub_metering_2 = sum(Sub_metering_2), 
            Sub_metering_3 = sum(Sub_metering_3),
            Sub_unnumbered = sum(Sub_unnumbered)) %>% 
  gather(Sub_metering_1, 
         Sub_metering_2, 
         Sub_metering_3, 
         Sub_unnumbered, 
         key = Submeter, 
         value = "Monthly_power")

#Plot all data
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
       subtitle="Measured per submeter (in '000s kWh)")

