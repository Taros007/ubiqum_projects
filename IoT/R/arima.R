## Load libraries ================================
library(tidyverse)
library(lubridate)
library(forecast)
library(ggfortify)
library(tseries)
library(doParallel)
source('./R/BBC_style.R')

## Load preprocessed data ========================
powerData <- readRDS('./output/powerData.RDS')

## Create data per week per submeter ==========================
powerWeek <- powerData %>% 
  group_by(year, month, day, week, weekday, day) %>% 
  summarise('meter1' = mean(Sub_metering_1),
            'meter2' = mean(Sub_metering_2), 
            'meter3' = mean(Sub_metering_3),
            'meternon' = mean(Sub_unnumbered),
            'total_energy_use' = mean(total_energy_use)) %>% 
  mutate(date = as_date(paste(year, month, day, sep="-"), "%Y-%m-%d"),
         #totalpower = meter1 + meter2 + meter3 + meternon) 
          totalpower = total_energy_use)
         
ts <- ts(powerWeek$meter1, start = decimal_date(as.Date(powerWeek$date[1])), end = decimal_date(as.Date(powerWeek$date[nrow(powerWeek)])), frequency = 365)

autoplot(ts)

train <- window(ts, end = c(2009, 365))
test <- window(ts, start = c(2010, 1))

## Step 1: visualize data ===============

ggplot(powerWeek, aes(date, totalpower)) + 
  geom_line() + 
  scale_x_date('month')  + 
  ylab("Total power usage") +
  xlab("")

## Step 2: decompose ======================================

decomp = stl(train, s.window="periodic")
deseasonal_poweruse <- seasadj(decomp)
plot(decomp)

## Step 3: test for stationarity ==========================

adf.test(train, alternative = "stationary") #p-value 0.01 -> stationary data

## Step 4: Autocorrelations and Choosing Model Order ======
Acf(deseasonal_poweruse, main='', lag.max = 365)
Pacf(deseasonal_poweruse, main='', lag.max = 365)

# count_d1 = diff(deseasonal_poweruse, differences = 1)
# plot(count_d1)
# adf.test(count_d1, alternative = "stationary")

## Step 5: ARIMA ===============================

#### finding the best K (fourier terms)
bestfit <- list(aicc=Inf)

cl <- makeCluster(3)
registerDoParallel(cl)
for(K in seq(8)) {   #precviously tested to max out at 7
  fit <- auto.arima(train,
                    xreg=fourier(train, K=K),
                    seasonal=T)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
} 
stopCluster(cl) 

fc <- forecast(bestfit,   
               xreg=fourier(test, K=bestK)) 

autoplot(fc) + autolayer(test)

plotting <- dplyr::bind_rows(
  data.frame(date=time(fc$fitted), Y=as.matrix(fc$fitted), datasort = "Training"), 
  data.frame(date=time(fc$mean), Y=as.matrix(fc$mean), datasort = "Forecast"),
  data.frame(date=time(test), Y = as.matrix(test), datasort = "Actual")
  )

plotting %>% ggplot(aes(x = date, y = Y, color = datasort)) + 
  geom_line() +
  scale_colour_manual(values = c("#1380A1", "#990000", "#FAAB18","#588300")) +
  bbc_style() +
  labs(title = "Energy use per day (in Wh)", subtitle = "Forecasted and actual for 2010")

accuracy(fc, test)[,2]
checkresiduals(fc)

## for monthly data
# powerMonth <- powerData %>% 
# group_by(year, month) %>% 
#   summarise('meter1' = mean(Sub_metering_1),
#             'meter2' = mean(Sub_metering_2), 
#             'meter3' = mean(Sub_metering_3),
#             'meternon' = mean(Sub_unnumbered)) %>% 
#   mutate(totalpower = meter1 + meter2 + meter3 + meternon) %>% 
#   ungroup()
# 
# tsMonth <- ts(powerMonth$totalpower, start = c(2006, 12), end = c(2010, 11), frequency = 12)
# 
# train <- window(tsMonth, end = c(2008, 1))
# test <- window(tsMonth, start = c(2010, 1))

###tbats
Acf(train)
tbatsFit <- tbats(train, use.parallel=TRUE, num.cores = 3) # fit tbats model
autoplot(forecast(tbatsFit, h = 365)) + autolayer(test) +
  ggtitle("Weekly energy prediction - Tbats") + theme_classic()# plot


## Multiple seasonality in TS =======================

train2 <- msts(train, seasonal.periods=c(7, 13, 365.25))
test2 <- msts(test, seasonal.periods=c(7, 13, 365.25))

taylor.fit <- tbats(train2)
autoplot(forecast(taylor.fit, h = 365)) + autolayer(test2) +
  ggtitle("Weekly energy prediction - Tbats & msts") + theme_classic()# plot
fc <- forecast(taylor.fit, h = 365)
checkresiduals(taylor.fit)

plotting <- dplyr::bind_rows(
  data.frame(date=time(train), Y=as.matrix(train), datasort = "Training"), 
  data.frame(date=time(fc$mean), Y=as.matrix(fc$mean), datasort = "Forecast"),
  data.frame(date=time(test2), Y = as.matrix(test2), datasort = "Actual")
)

plot <- plotting %>% ggplot(aes(x = date, y = Y, color = datasort)) + 
  geom_line() +
  scale_colour_manual(values = c("#1380A1", "#990000", "#FAAB18","#588300")) +
  bbc_style() +
  theme(axis.text.y = element_blank()) +
  labs(title = "Energy use per day", subtitle = "Forecasted and actual for 2010")

finalise_plot(plot, "UCI (energy data)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/tbats_msts_modelling_all.jpg')

## Holt Winters ============================================
HW <- HoltWinters(train)                  
autoplot(forecast(HW, h = 365)) + autolayer(test) +
  ggtitle("Weekly energy prediction - HoltWinters") + theme_classic()# plot
checkresiduals(HW)
