## Load libraries ================================
library(tidyverse)
library(magrittr)
library(datetime)
library(lubridate)
library(scales)
library(corrr)
source('./R/BBC_style.R')

## Load preprocessed data ========================
powerData <- readRDS('./output/powerData.RDS')

## Check start and end dates for every year =====================
group_by(powerData, year) %>% 
  summarize(min = min(DateTime), max = max(DateTime))

## Calculate average temperature per day ===============================
# Load weather data
weather <- readRDS('./input/weatherinfo.RDS')
names(weather)[names(weather) == 'Date'] <- 'date'

weather$avg_temp <- rowMeans(select(weather,hour0:hour23))
powerData$avg_temp <- as.numeric(weather$avg_temp[match(date(powerData$DateTime), weather$date)])

## Calculate total charges for electricity use ==============
# Load historical rates (source: Eurostat)
rates <- read_csv('./resources/nrg_pc_204_1_Data.csv')

#Filter, and convert custom date format to normal dates
rates %<>% 
  filter(GEO == "France", 
         TAX == "All taxes and levies included", 
         CURRENCY == "Euro") %>% 
  mutate(
    TIME =  semester(as_date(ifelse(month(yq(TIME)) == 4, yq(TIME) + months(3), yq(TIME))), with_year = T)
  ) %>% 
  select(TIME, Value) %>% 
  filter(TIME != "2007.1") %>% 
  rbind(tribble(~"TIME", ~"Value",
                "2006.2", 0.1200,
                "2007.1", 0.1218))

#Calculate energy cost per minute
#z <- with(rates$Value, rates[match(semester(powerData$DateTime, with_year = T), rates$TIME)])
powerData$rate <- as.numeric(
  rates$Value[match(semester(powerData$DateTime, with_year = T), rates$TIME)])
powerData$costs <- powerData$rate / 1000 * powerData$Global_active_power * 1000 / 60

## Plot use over all years ===============================
graphData <- powerData %>% 
  group_by(year, month) %>% 
  summarise(Kitchen = sum(Sub_metering_1),
            'Laundry Room' = sum(Sub_metering_2), 
            'Water heater & A/C' = sum(Sub_metering_3),
            'Non-submetered' = sum(Sub_unnumbered),
            'Average Temperature' = mean(avg_temp)) %>% 
  gather(Kitchen, 
         'Laundry Room', 
         'Water heater & A/C', 
         'Non-submetered', 
         key = Submeter, 
         value = "Monthly_power")

plot <- ggplot(graphData, 
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

finalise_plot(plot, "UCI (energy data)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/use_over_years.jpg')

powerData %>% 
  group_by(year) %>%
  summarize(Energy_use = sum(total_energy_use))


## Plot energy use over 24 hrs ===============================
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

plot <- ggplot(graphData, 
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

finalise_plot(plot, "UCI (energy data)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/daily_profile.jpg')
  
## Plot energy use per month =============================
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

plot <- ggplot(graphData, 
       aes(x = as.Date(paste("2012",month,"1", sep="-"),origin="2012-01-01"), 
           y = Monthly_power, 
           colour = Submeter)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300")) +
  bbc_style() +
  scale_x_date(date_labels = "%b",date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Average energy use during the year",
       subtitle="Measured per submeter in Wh")

finalise_plot(plot, "UCI (energy data)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/use_per_month.jpg')

## Plot electricity use per weekday ==================
graphData <- powerData %>% 
  group_by(weekday) %>% 
  summarise(Kitchen = mean(Sub_metering_1),
            'Laundry Room' = mean(Sub_metering_2), 
            'Water heater & A/C' = mean(Sub_metering_3),
            'Non-submetered' = mean(Sub_unnumbered)
            ) %>% 
  gather(Kitchen, 
         'Laundry Room', 
         'Water heater & A/C', 
         'Non-submetered', 
         key = Submeter, 
         value = "Weekday_power")

plot <- ggplot(graphData, 
       aes(x = weekday, 
           y = Weekday_power, 
           group = Submeter,
           colour = Submeter)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300")) +
  bbc_style() +
  scale_x_discrete(labels = function(x) {x <- substr(x, 1, 3)}) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Energy profile over the week",
       subtitle="Measured per submeter in Wh")

finalise_plot(plot, "UCI (energy data)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/use_per_weekday.jpg')

## Plot electricity rates ==================
graphData <- rates
graphData$Value <- as.numeric(graphData$Value)
graphData$TIME <- as.numeric(graphData$TIME)

plot <- ggplot(graphData, aes(x = TIME, y = Value)) +
  geom_smooth(se = F) + 
  labs(title = "Electricity prices in France",
       subtitle = "2007 - 2018") +
  ylab("Price per kWh") +
  theme(axis.title.y=element_blank()) +
  geom_hline(aes(yintercept = 0), linetype="dotted") +
  #scale_y_continuous(breaks = seq(0.0, 0.40, by = 0.05)) +
  scale_y_continuous(limits = c(0,0.20), labels = dollar_format(suffix = "", prefix = "€")) +
  scale_x_continuous(breaks = c(2007:2018)) +
  bbc_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# geom_label(aes(x = 2014, y = 0.14, label = "60% price increase"),
#            hjust = 0,
#            vjust = 0.5,
#            lineheight = 0.8,
#            colour = "#fb2563",
#            fill = "white",
#            label.size = NA,
#            family="Helvetica",
#            size = 12)

finalise_plot(plot, "Eurostat", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/prices_electricity.jpg')

## Plot total costs electricity ==================
graphData <- powerData %>% 
  group_by(year, month) %>% 
  summarise(costs = sum(costs))

plot <- ggplot(graphData, 
       aes(x = as.Date(paste(year, month, "01", sep="-")), 
           y = costs
           )) +
  geom_line(size = 1, colour = "#FAAB18") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  scale_x_date(breaks = pretty_breaks(10), labels = date_format("%b-%g")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="Monthly energy costs",
       subtitle="(in €)")

finalise_plot(plot, "UCI (energy data), EuroStat (electricity costs)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/total_costs.jpg')

## Scatter weather vs energy use ===================

plot <- powerData %>% 
  group_by(month, day) %>% 
  summarise(Kitchen = mean(Sub_metering_1),
            'Laundry Room' = mean(Sub_metering_2), 
            'Water heater & A/C' = mean(Sub_metering_3),
            'Non-submetered' = mean(Sub_unnumbered),
            avg_temp = mean(avg_temp)) %>% 
  correlate() %>% 
  focus(avg_temp) %>%
  mutate(rowname = reorder(rowname, avg_temp)) %>%
  filter(!rowname %in% c("month", "day")) %>% 
  ggplot(aes(rowname, avg_temp)) +
    geom_col(fill = "#1380A1") + 
    coord_cartesian(xlim = c(-1, 1)) + 
    coord_flip() +
    labs(title="Correlation between temperature and electricity use",
         subtitle="measured per submeter") +
    ylab("Correlation") +
    theme(axis.title.y=element_blank()) +
    bbc_style() +
    geom_hline(aes(yintercept = 0), linetype="dotted")

finalise_plot(plot, "Darksky (weather) & UCI (energy use)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/correlation_weather.jpg')

graphData <- powerData %>% 
  group_by(month, day) %>% 
  summarise(Total_energy_consumption = mean(total_energy_use),
            avg_temp = mean(avg_temp))

#Color points by the first principal component
graphData$pc <- predict(prcomp(~avg_temp+Total_energy_consumption, graphData))[,1]

plot <- ggplot(graphData, aes(avg_temp, Total_energy_consumption, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = .7) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  bbc_style() +
  labs(title="Average daily temperature vs daily electricity usage",
       subtitle="Daily energy use in kWh") +
  xlab("Average daily temperature (in °C)") +
  theme(axis.title.y=element_blank()) +
  geom_hline(aes(yintercept = 0), linetype="dotted")

finalise_plot(plot, "Darksky (weather) & UCI (energy use)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/temp_vs_energy.jpg')


